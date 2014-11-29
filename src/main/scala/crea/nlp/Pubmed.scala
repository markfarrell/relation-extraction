package crea.nlp

import scala.xml.XML

import scalaz._
import scalaz.concurrent._
import scala.concurrent.duration._
import scalaz.stream._
import Scalaz._

import java.io.{PrintStream, OutputStream}

import epic.preprocess.MLSentenceSegmenter

import twitter4j._

import Terms._

object Pubmed {

  private[this] implicit val scheduler = scalaz.stream.DefaultScheduler

  private[this] final case class Pubmed(pmid : String, title : String, _abstract : List[String])

  private[this] final case class Row(pmid : String, subject : String, predicate : String, obj : String,
    term : String, elapsed : Long, timestamp : Long) {

    def toCSV : String = s""""${pmid}","${predicate}","${subject}","${obj}","${term}","${elapsed}","${timestamp}"\n"""

    def toTweet : String = s"""True or false? ${predicate}(${hashtag(subject)}, ${hashtag(obj)}) ${url} ${hashtag(term)}"""

    def toJSON : String = s"""{"pmid":"${pmid}","predicate":"${predicate}","subject":"${subject}","obj":"${obj}","term":"${term}","elapsed":"${elapsed}","timestamp":"${timestamp}"}"""

    private[this] lazy val url : String = Bitly(s"http://www.ncbi.nlm.nih.gov/pubmed/${pmid}").or(Task.now("")).run

    private[this] def hashtag(s : String) = if(s.matches("""^\d+""")) {
      s
    } else {
      "#" + s.replaceAll("\\W", " ")
        .split(" ")
        .map(_.capitalize)
        .mkString("")
    }

  }

  private[this] val retmax = 5000
  private[this] val timeout = 180000
  private[this] val bufferSize = 128

  private[this] val t = async.topic[String]()

  def apply(file : String) : Task[Unit] = {

    val fileTopic = async.topic[(String, String)]()

    val fileIn = fileTopic.subscribe

    val fileSink = io.channel { (term : String) => Task.delay {

      search(term, fileTopic).run.runAsync(_ => ())

    }}

    io.linesR(file)
      .to(fileSink)
      .run.run

    val src = Web.in.merge(IRC.in).merge(fileIn)

    src.observe(io.stdOutLines.contramap(_.toString))
     .flatMap((article _).tupled)
     .observe(Twitter.out.contramap(_.toTweet))
     .observe(io.stdOutLines.contramap(_.toCSV))
     .observe(t.publish.contramap(_.toJSON))
     .map(_.toCSV)
     .pipe(text.utf8Encode)
     .to(io.fileChunkW(s"${System.currentTimeMillis}.csv", bufferSize))
     .run

  }

  def in : Process[Task, String] = t.subscribe

  def search(term : String, t : async.mutable.Topic[(String, String)]) : Process[Task, Unit] = {

    ids(term)
      .map(id => (id, term))
      .zipWith(Process.awakeEvery(10 seconds))((x, _) => x)
      .to(t.publish)

  }

  private[this] def ids(term : String) : Process[Task, String] = {

    def xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="${term.replaceAll(" ", "%20")}"&retmax=${retmax}&rettype=xml""")

    val lst = (xml \\ "eSearchResult" \\ "IdList" \\ "Id").map(_.text).toList

    Process.emitAll(lst)

  }

  private[this] def writer1[I](n : Int) : Writer1[Seq[I], I, I] = {

    import process1._
    import Process._

    require(n > 0, "window size must be > 0, was: " + n)

    def go(window: Vector[I]): Writer1[Vector[I], I, I] = {
      emit(-\/(window)) fby receive1(i => emit(\/-(i)) ++ go(window.tail :+ i))
    }

    chunk(n).once.flatMap(go)

  }

  private[this] def article(id : String, term : String) : Process[Task, Row] = Process.eval { Task {

    val tokens = MLSentenceSegmenter.bundled().get

    val xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=${id}&rettype=xml""")

    val seq = (xml \\ "PubmedArticleSet" \\ "PubmedArticle").map { article =>

      val pmid = (article \\ "PMID").text
      val title = (article \\ "ArticleTitle").text
      val _abstractBlock = (article \\ "Abstract").text

      val _abstract = tokens(Option(_abstractBlock).getOrElse("")).toList

      Pubmed(pmid, title, _abstract)

    }

    assert(seq.length === 1, "Sequence length not 1!")

    seq(0)

  } }.flatMap(compileArticle)
    .flatMap(Process.emitAll)
    .filter(_._2.args.length === 2)
    .map { case (pmid, relation, elapsed) => Row(pmid,
      relation.args.head.id,
      relation.literal.id,
      relation.args.last.id,
      term,
      elapsed,
      System.currentTimeMillis)
  }

  private[this] def compileArticle(article : Pubmed) : Process[Task, List[(String, Relation, Long)]] = Process.emitAll(article._abstract)
    .flatMap(compileSentence(article))

  private[this] def compileSentence(article : Pubmed)(sentence : String) : Process[Task, List[(String, Relation, Long)]] = Process.eval { Task {

    val t1 = System.currentTimeMillis
    val relations = Compile(parse(sentence)).toList.flatten
    val t2 = System.currentTimeMillis
    val dt = t2 - t1

    relations.map(x => (article.pmid, x, dt))

  }.timed(timeout).or(Task.now(List.empty[(String, Relation, Long)])) }

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

object Twitter {

  private[this] lazy val twitter = TwitterFactory.getSingleton

  def out : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      twitter.updateStatus(s)
      ()
    }.or(Task.delay(println(s"Could not tweet: ${s}")))
  }

}

object IRC {

  import org.jibble.pircbot._

  private[this] implicit val scheduler = scalaz.stream.DefaultScheduler

  private[this] val t = async.topic[(String, String)]()

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

            Pubmed.search(term, t).run.run

          case _ =>

            this.sendMessage(channel, s"${sender}: I don't understand.")

        }

      }.runAsync(_ => ())

    }

  }

  def in : Process[Task, (String, String)] = t.subscribe

  def out(channelName : String) : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      bot.sendMessage(channelName, s)
    }.or(Task.delay(println(s"Could not IRC: ${s}")))
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

  private[this] val t = async.topic[(String, String)]()

  private[this] val route = HttpService {

    case req@ GET -> Root =>

      val src = Pubmed.in.map(s => Text(s))

      val sink : Sink[Task, WebSocketFrame] = Process.constant {

        case Text(term, _) => Task.delay {

          Pubmed.search(term, t).run.runAsync(_ => ())

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
