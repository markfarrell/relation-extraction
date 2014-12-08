package crea.nlp

import scala.xml.XML
import scala.xml.Elem
import scala.util.Random

import scalaz._
import scalaz.concurrent._
import scala.concurrent.duration._
import scalaz.stream._
import Scalaz._

import java.io.{PrintStream, OutputStream}

import epic.preprocess.MLSentenceSegmenter

import org.log4s._

import twitter4j._

import Terms._

object Pubmed {

  private[this] implicit val scheduler = scalaz.stream.DefaultScheduler

  private[this] implicit val logger = org.log4s.getLogger

  final case class Row(pmid : String, subject : String, predicate : String, obj : String,
    term : String, elapsed : Long, timestamp : Long) {

    def toCSV : String = s""""${pmid}","${predicate}","${subject}","${obj}","${term}","${elapsed}","${timestamp}"\n"""

    def toTweet : String = s"""True or false? ${predicate}(${hashtag(subject)}, ${hashtag(obj)}) ${url} ${hashtag(term)}"""

    def toJSON : String = s"""{"pmid":"${pmid}","predicate":"${predicate}","subject":"${subject}","obj":"${obj}","term":"${term}","elapsed":"${elapsed}","timestamp":"${timestamp}"}"""

    private[this] lazy val url : String = Bitly(s"http://www.ncbi.nlm.nih.gov/pubmed/${pmid}").or(Task.now("")).run

    private[this] def hashtag(s : String) = {

      val ret = s.replaceAll("\\W", " ")
        .split(" ")
        .map(_.capitalize)
        .mkString("")

      if(ret.matches("""^\d+$""")) {

        ret

      } else {

        "#" + ret

      }

    }

  }

  private[this] val bufferSize = 128

  private[this] val whitelist = List("increase", "decrease", "upregulate", "downregulate",
    "regulate", "encode", "decode", "secrete", "block", "activate", "inhibit", "trigger", "signal",
    "induce", "transmit", "cause", "treat", "prevent",  "interact", "suppress", "mediate", "respond",
    "translate", "approve", "link", "correlate", "inject", "release", "express",
    "bind", "stimulate")

  private[this] val t = async.topic[String]()

  def apply(file : String) : Task[Unit] = {

    val fileIn = nondeterminism.njoin(maxOpen = 10, maxQueued = 4)(io.linesR(file).map(search))

    val src = Web.in.merge(IRC.in).merge(fileIn)

    logger.debug("Begin reading relations.")

    src.observe(Log.info.contramap(_.toCSV))
      .filter(row => whitelist.contains(row.predicate))
      .observe(Twitter.out.contramap(_.toTweet))
      .observe(t.publish.contramap(_.toJSON))
      .map(_.toCSV)
      .pipe(text.utf8Encode)
      .to(io.fileChunkW(s"${System.currentTimeMillis}.csv", bufferSize))
      .run

  }

  def in : Process[Task, String] = t.subscribe

  def search(term : String) : Process[Task, Row] = {

    ids(term)
     .map(id => (id, term))
     .flatMap((extractArticle _).tupled)

  }

  /**
    * E-utilities guide: http://www.ncbi.nlm.nih.gov/books/NBK25499/
   **/
  def ids(term : String) : Process[Task, String] = {

    val datetype = "pdat"
    val mindate = "2000/01/01"
    val maxdate = "2015/01/01"
    val retmax = 100000
    val formattedTerm = term.replaceAll(" ", "%20")
    val duration = 5 minutes

    def xml = {

      import sys.process._
      import java.io.File
      import java.net.URL

      val url = new URL(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="${formattedTerm}"&retmax=${retmax}&rettype=xml&datetype=${datetype}&mindate=${mindate}&maxdate=${maxdate}""")

      val file = new File(s"""data/${term.replace(" ", "")}.xml""")

      logger.debug(s"Downloading ${file.getPath}")

      url #> file !!

      logger.debug(s"Begin parsing ${file.getPath}")

      val ret = XML.loadFile(file)

      logger.debug(s"Finished parsing ${file.getPath}")

      ret

    } 

    def seq(elem : Elem) : Seq[String] = Random.shuffle((elem \\ "eSearchResult" \\ "IdList" \\ "Id").map(_.text).toSeq)

    def task : Task[Elem] = Task(xml).timed(duration).or(reattempt).onFinish { 

      case Some(throwable) => 

        Task.delay(logger.error(throwable)(s"Fetching ids for ${term} failed"))

      case None => 

        Task.delay(logger.debug(s"Finished downloading ids for ${term}"))

    }

    def reattempt : Task[Elem] = Task.delay { 

      logger.warn(s"Reattempting to download article (${term}, ${id}).")

      task.run

    } 

    Process.eval(task)
      .pipe(process1.lift(seq))
      .observe(Log.info.contramap(x => s"Got ${x.size} article ids about '${term}'."))
      .pipe(process1.unchunk)

  }

  def extractArticle(id : String, term : String) : Process[Task, Row] = {

    def tokens = MLSentenceSegmenter.bundled().get
    val duration = 2 minutes

    def xml = { 

      import sys.process._
      import java.io.File
      import java.net.URL
      
      val url = new URL(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=${id}&rettype=xml""")

      val file = new File(s"data/${id}.xml")

      logger.debug(s"Downloading ${file.getPath}")

      url #> file !!

      logger.debug(s"Begin parsing ${file.getPath}")

      val ret = XML.loadFile(file)

      logger.debug(s"Finished parsing ${file.getPath}")

      ret

    } 

    def seq(elem : Elem) : Seq[(String, String, String)] = (elem \\ "PubmedArticleSet" \\ "PubmedArticle").flatMap { article =>

      val pmid = (article \\ "PMID").text
      val title = (article \\ "ArticleTitle").text
      val _abstractBlock = (article \\ "Abstract").text

      logger.debug(s"Begin tokenizing ${pmid}")

      val sentences = tokens(Option(_abstractBlock).getOrElse("")).toList

      logger.debug(s"Finished tokenizing ${pmid}")

      sentences.map(sentence => (pmid, title, sentence))

    }

    def extract : ((String, String, String)) => Seq[Row] = {
      case (pmid, title, sentence) =>

        logger.debug(s"Begin extracting '${sentence}'")

        val timeout = 3 minutes
        val t1 = System.currentTimeMillis
        val res = Task(Compile(Parse(sentence))).timed(timeout).attemptRun
        val t2 = System.currentTimeMillis
        val dt = t2 - t1

        res match {

          case \/-(\/-(relations)) =>

            relations.filter(_.args.length == 2)
              .map { relation =>

                val predicate = relation.literal.id
                val subject = relation.args.head.id
                val obj = relation.args.last.id
                val now = System.currentTimeMillis

                val row = Row(pmid, subject, predicate, obj, term, dt, now)

                logger.debug(s"Extracted: ${row.toCSV}")

                row

              }

          case _ =>

            logger.warn(s"Could not extract relations from: ${sentence} | ${title} | ${pmid} | ${dt} | ${term}")

            Seq()

        }

    }

   def task : Task[Elem] = Task(xml).timed(duration).or(reattempt).onFinish { 

      case Some(throwable) => 

        Task.delay(logger.error(throwable)(s"Fetching article for (${term}, ${id}) failed"))

      case None => 

        Task.delay(logger.debug(s"Finished downloading article for (${term}, ${id})"))

    }

    def reattempt : Task[Elem] = Task.delay { 

      logger.warn(s"Reattempting to download article (${term}, ${id}).")

      task.run

    } 

    Process.eval(task)
      .pipe(process1.lift(seq))
      .pipe(process1.unchunk)
      .pipe(process1.lift(extract))
      .pipe(process1.unchunk)

  }

}

private[this] object Bitly {

  def apply(link : String) : Task[String] = Task {

    import scala.io.Source
    import scala.util.parsing.json._

    val result = Source.fromURL(requestURL(link)).mkString

    JSON.parseFull(result).map {
      _.asInstanceOf[Map[String,Any]]("data")
      .asInstanceOf[Map[String, Any]]("url")
      .asInstanceOf[String]
    }.get

  }

  private[this] def requestURL(link : String) = {
    s"""https://api-ssl.bitly.com/v3/shorten?login=o_1h56l570kl&apiKey=R_43026c0ecd0849c090ae4547b46ac1d7&longUrl=${link}"""
  }

}

object Log {

  def debug(implicit logger : org.log4s.Logger) : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      logger.debug(s)
    }
  }

  def info(implicit logger : org.log4s.Logger) : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      logger.info(s)
    }
  }

  def warn(implicit logger : org.log4s.Logger) : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      logger.warn(s)
    }
  }

  def error(implicit logger : org.log4s.Logger) : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      logger.error(s)
    }
  }

  def trace(implicit logger : org.log4s.Logger) : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      logger.trace(s)
    }
  }

}

object Twitter {

  private[this] lazy val twitter = TwitterFactory.getSingleton

  private[this] implicit val logger = org.log4s.getLogger

  def out : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      twitter.updateStatus(s)
      ()
    }.or(Task.delay(logger.warn(s"Could not tweet: ${s}")))
  }

}

object IRC {

  import org.jibble.pircbot._

  private[this] implicit val scheduler = scalaz.stream.DefaultScheduler

  private[this] val logger = org.log4s.getLogger

  private[this] val t = async.topic[Pubmed.Row]()

  private[this] val bot = new PircBot {

    private[this] val name = "semanticbot"

    private[this] val pattern = s"""^${name}: research (.+)""".r

    this.setName(name)
    this.setVerbose(false)
    this.connect("irc.freenode.net")
    this.joinChannel("###cmc")
    this.joinChannel("#crea")

    override def onMessage(channel : String, sender : String, login : String,
      hostname : String, message : String) : Unit = if(message.startsWith(name)) {

      Task {

        message match {

          case pattern(term) =>

            this.sendMessage(channel, s"Researching ${term}.")

            Pubmed.search(term).to(t.publish).run.run

          case _ =>

            this.sendMessage(channel, s"${sender}: I don't understand.")

        }

      }.runAsync(_ => ())

    }

  }

  def in : Process[Task, Pubmed.Row] = t.subscribe

  def out(channelName : String) : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      bot.sendMessage(channelName, s)
    }.or(Task.delay(logger.warn(s"Could not IRC: ${s}")))
  }

}

object Web {

  import org.http4s._
  import org.http4s.dsl._
  import org.http4s.websocket._
  import org.http4s.websocket.WebsocketBits._
  import org.http4s.server._
  import org.http4s.server.websocket._

  import org.http4s.server.jetty.JettyBuilder
  import org.http4s.server.blaze.{WebSocketSupport, Http1ServerStage}
  import org.http4s.blaze.channel.nio1.NIO1SocketServerChannelFactory
  import org.http4s.blaze.channel.SocketConnection
  import org.http4s.blaze.pipeline.LeafBuilder
  import java.nio.ByteBuffer
  import java.net.InetSocketAddress

  private[this] implicit val scheduler = scalaz.stream.DefaultScheduler

  private[this] val t = async.topic[Pubmed.Row]()

  private[this] val route = HttpService {

    case req@ GET -> Root =>

      val src = Pubmed.in.map(s => Text(s))

      val sink : Sink[Task, WebSocketFrame] = Process.constant {

        case Text(term, _) => Task.delay {

          Pubmed.search(term).to(t.publish).run.runAsync(_ => ())

        }

        case f =>

          Task.delay(println(s"Unknown type: $f"))

      }

      WS(src, sink)

  }

  def in = t.subscribe

  def start(file : String = "neuroendocrine.txt") : Unit = {

    Task(Pubmed(file).run).runAsync(_ => ())

    def pipebuilder(conn: SocketConnection): LeafBuilder[ByteBuffer] = new Http1ServerStage(route, Some(conn)) with WebSocketSupport

    new NIO1SocketServerChannelFactory(pipebuilder, 12, 8*1024)
      .bind(new InetSocketAddress(8080))
      .run()

  }

}
