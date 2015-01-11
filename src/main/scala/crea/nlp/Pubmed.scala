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
import java.util.Date
import java.text.SimpleDateFormat

import epic.preprocess.MLSentenceSegmenter

import org.log4s._

import twitter4j._

import Terms._

object Pubmed {

  private[this] implicit val scheduler = scalaz.stream.DefaultScheduler

  private[this] implicit val logger = org.log4s.getLogger

  final case class Row(pmid : String, subject : String, predicate : String, obj : String,
    term : String, timestamp : Long) {

    def toCSV : String = s""""${pmid}","${predicate}","${subject}","${obj}","${term}","${timestamp}"\n"""

    def toTweet : String = s"""True or false? ${predicate}(${hashtag(subject)}, ${hashtag(obj)}) ${url} ${hashtag(term)}"""

    def toJSON : String = s"""{"pmid":"${pmid}","predicate":"${predicate}","subject":"${subject}","obj":"${obj}","term":"${term}","timestamp":"${timestamp}"}"""

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
    "bind", "stimulate", "transduce", "excite")

  private[this] val t = async.topic[String]()

  def apply(file : String) : Task[Unit] = {

    val src = nondeterminism.njoin(maxOpen = 10, maxQueued = 4)(io.linesR(file).map(search))

    logger.debug("Begin reading relations.")

    src.observe(Log.info.contramap(_.toCSV))
      .filter(row => DBpedia.contains(row.subject) && DBpedia.contains(row.obj))
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

    def seq(elem : Elem) : Seq[(String, String, String, Long)] = (elem \\ "PubmedArticleSet" \\ "PubmedArticle").flatMap { article =>

      val pmid = (article \\ "PMID").text
      val title = (article \\ "ArticleTitle").text
      val _abstractBlock = (article \\ "Abstract").text

      val timestamp : Long = Task {

        (article \\ "MedlineCitation" \\ "DateCreated").map { dateElem =>

          val year = (dateElem \\ "Year").text
          val month = (dateElem \\ "Month").text
          val day = (dateElem \\ "Day").text

          val sdf = new SimpleDateFormat("dd/M/yyyy")

          val date = sdf.parse(s"${day}/${month}/${year}")

          logger.debug(s"(${term}, ${pmid}) was created on ${date.toString}")

          date.getTime

        }.max

      }.onFinish {

        case Some(throwable) =>

          Task(logger.error(throwable)(s"Failed to extraction a date for article (${pmid}, ${term})"))

        case None => Task.now()

      }.or(Task.now(0L)).run

      val sentences = tokens(Option(_abstractBlock).getOrElse("")).toList

      sentences.map(sentence => (pmid, title, sentence, timestamp))

    }

    def extract : ((String, String, String, Long)) => Seq[Row] = {
      case (pmid, title, sentence, timestamp) =>

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

                val row = Row(pmid, subject, predicate, obj, term, timestamp)

                logger.debug(s"Extracted(${dt}): ${row.toCSV}")

                row

              }

          case _ =>

            logger.warn(s"Could not extract relations from(${dt}): ${sentence} | ${title} | ${pmid} | ${term}")

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

private[this] object Log {

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

private[this] object DBpedia {

  import scalaj.http._

  private[this] implicit val logger = org.log4s.getLogger

  private[this] val endpoint = "http://dbpedia.org/sparql"
  private[this] val defaultGraphURI = "http://dbpedia.org"
  private[this] val format = "json"
  private[this] val timeout = "30000"
  private[this] val debug = "on"

  // Expected response if a resource exists.
  private[this] val expect = """{"head":{"link":[]},"boolean":true}"""

  /**
    * Does a resource exist on DBpedia?
    * e.g. DBPedia.contains("ghrelin") returns true.
   **/
  def contains(resource : String) : Boolean = {

    // e.g. blue man -> Blue_man
    val formattedResource = resource.replaceAll(" ", "_").capitalize

    val query = s"""ASK { dbpedia:$formattedResource ?p ?o }"""

    val res = Task {

      Http(endpoint).param("default-graph-uri", defaultGraphURI)
        .param("query", query)
        .param("format", format)
        .param("timeout", timeout)
        .param("debug", debug)
        .asString
        .body
        .replaceAll(" ", "")

    }.attemptRun

    res match {

      case \/-(json) if json === expect =>

        logger.debug(s"Found resource dbpedia:$formattedResource")

        true

      case _ =>

        logger.warn(s"Could not find resource dbpedia:$formattedResource")

        false

    }


  }

}

private[this] object Twitter {

  private[this] lazy val twitter = TwitterFactory.getSingleton

  private[this] implicit val logger = org.log4s.getLogger

  def out : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      twitter.updateStatus(s)
      ()
    }.or(Task.delay(logger.warn(s"Could not tweet: ${s}")))
  }

}

