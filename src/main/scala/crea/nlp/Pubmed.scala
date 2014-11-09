package crea.nlp

import scala.xml.XML

import scalaz._
import scalaz.concurrent._
import scalaz.stream._
import Scalaz._

import java.io.{PrintStream, OutputStream}

import epic.preprocess.MLSentenceSegmenter

import twitter4j._

import Terms._

object Pubmed {

  private[this] final case class Pubmed(pmid : String, title : String, _abstract : List[String])
  final case class Row(pmid : String, subject : String, predicate : String, obj : String,
    term : String, elapsed : Long, timestamp : Long) {

    def toCSV : String = s""""${pmid}","${predicate}","${subject}","${obj}","${term}","${elapsed}","${timestamp}""""

    def toTweet : String = s"""True or false? ${predicate}(#${camel(subject)}, #${camel(obj)}) (${url}) #${camel(term)}"""

    def url : String = Bitly(s"http://www.ncbi.nlm.nih.gov/pubmed/${pmid}")

    private[this] def camel(s : String) = s.split(" ").map(_.capitalize).mkString("")

  }

  private[this] val retmax = 5000
  private[this] val timeout = 180000
  private[this] val maxOpen = 3

  def apply() : Task[Unit] = stream.merge.mergeN(maxOpen)(io.linesR("terms.txt").map(mine))
    .observe(Twitter.out.contramap(_.toTweet))
    .observe(io.stdOutLines.contramap(_.toCSV))
    .map(_.toCSV)
    .intersperse("\n")
    .pipe(text.utf8Encode)
    .to(io.fileChunkW(s"${System.currentTimeMillis}.csv", bufferSize = 128))
    .run

  def mine(term : String) : Process[Task, Row] = stream.merge.mergeN(1)(articles(term))
    .flatMap(compileArticle)
    .flatMap(lst => Process.emitAll(lst))
    .filter(_._2.args.length === 2)
    .map { case (pmid, relation, elapsed) => Row(pmid,
      relation.args.head.id,
      relation.literal.id,
      relation.args.last.id,
      term,
      elapsed,
      System.currentTimeMillis)
    }

  private[this] def article(id : String) : Process[Task, Pubmed] = Process.eval { Task {

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

  } }

  private[this] def articles(term : String) : Process[Task, Process[Task, Pubmed]] = {

    def xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="${term}"&retmax=${retmax}&rettype=xml""")

    def ids = (xml \\ "eSearchResult" \\ "IdList" \\ "Id").map(_.text).toList

    Process.emitAll(ids.map(article)).toSource

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

  def apply(link : String) : String = {

    import scala.io.Source
    import scala.util.parsing.json._

    val result = Source.fromURL(requestURL(link)).mkString

    JSON.parseFull(result).map(_.asInstanceOf[Map[String,Any]]("data").asInstanceOf[Map[String, Any]]("url").asInstanceOf[String]).get

  }

  private[this] def requestURL(link : String) = {
    s"""https://api-ssl.bitly.com/v3/shorten?login=o_1h56l570kl&apiKey=R_43026c0ecd0849c090ae4547b46ac1d7&longUrl=${link}"""
  }

}

object Twitter {

  def out : Sink[Task, String] = io.channel { (s : String) =>
    Task.delay {
      TwitterFactory.getSingleton.updateStatus(s)
      ()
    }.or(Task.delay(println(s"Could not tweet: ${s}")))
  }

}
