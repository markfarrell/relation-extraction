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
    "translate", "approve", "link", "correlate", "inject", "release")

  private[this] val t = async.topic[String]()

  def apply(file : String) : Task[Unit] = {

    val fileTopic = async.topic[Row]()

    val fileIn = fileTopic.subscribe

    val fileSink = io.channel { (term : String) => Task.delay {

      search(term).to(fileTopic.publish).run.runAsync(_ => ())

    }}

    io.linesR(file)
      .to(fileSink)
      .run.run

    val src = Web.in.merge(IRC.in).merge(fileIn)

    src.filter(row => whitelist.contains(row.predicate))
     .observe(Twitter.out.contramap(_.toTweet))
     .observe(Log.info.contramap(_.toCSV))
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

    def xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="${formattedTerm}"&retmax=${retmax}&rettype=xml&datetype=${datetype}&mindate=${mindate}&maxdate=${maxdate}""")

    def seq(elem : Elem) : Seq[String] = Random.shuffle((elem \\ "eSearchResult" \\ "IdList" \\ "Id").map(_.text).toSeq)

    Process.eval(Task.delay(xml).timed(duration))
      .pipe(process1.lift(seq))
      .observe(Log.debug.contramap(x => s"Got ${x.size} article ids about '${term}'."))
      .pipe(process1.unchunk)

  }

  def extractArticle(id : String, term : String) : Process[Task, Row] = {

    val tokens = MLSentenceSegmenter.bundled().get
    val duration = 5 minutes

    def xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=${id}&rettype=xml""")

    def seq(elem : Elem) : Seq[(String, String, String)] = (elem \\ "PubmedArticleSet" \\ "PubmedArticle").flatMap { article =>

      val pmid = (article \\ "PMID").text
      val title = (article \\ "ArticleTitle").text
      val _abstractBlock = (article \\ "Abstract").text

      val sentences = tokens(Option(_abstractBlock).getOrElse("")).toList

      sentences.map(sentence => (pmid, title, sentence))

    }

    def extract : ((String, String, String)) => Seq[Row] = {
      case (pmid, title, sentence) =>

        val t1 = System.currentTimeMillis
        val res = Compile(parse(sentence))
        val t2 = System.currentTimeMillis
        val dt = t2 - t1

        res match {

          case -\/(_) =>

            logger.warn(s"Could not extract relations from: ${sentence} | ${title} | ${pmid} | ${dt}")

            Seq()

          case \/-(relations) =>

            relations.filter(_.args.length == 2)
              .map { relation =>

                val predicate = relation.literal.id
                val subject = relation.args.head.id
                val obj = relation.args.last.id
                val now = System.currentTimeMillis

                Row(pmid, subject, predicate, obj, term, dt, now)

              }

        }
    }

    Process.eval(Task.delay(xml).timed(duration))
      .pipe(process1.lift(seq))
      .pipe(process1.unchunk)
      .pipe(process1.lift(extract))
      .pipe(process1.unchunk)

  }

  private[this] def parse(sentence : String) : Tree[String] = (new Parser).apply(sentence)

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
