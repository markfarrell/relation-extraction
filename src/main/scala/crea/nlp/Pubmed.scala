package crea.nlp

import scala.xml.XML

import scalaz._
import scalaz.concurrent._
import scalaz.stream._
import Scalaz._

import java.io.{PrintStream, OutputStream}

import epic.preprocess.MLSentenceSegmenter

import Terms._

object Pubmed {

  private[this] final case class Pubmed(pmid : String, title : String, _abstract : List[String])
  private[this] final case class Row(pmid : String, subject : String, predicate : String, obj : String,
    term : String, elapsed : Long, timestamp : Long) {
    override def toString : String = s""""${pmid}","${predicate}","${subject}","${obj}","${term}","${elapsed}","${timestamp}""""
  }

  private[this] val retmax = 5000
  private[this] val timeout = 90000
  private[this] val maxOpen = 3

  private[this] val nullStream = new PrintStream(new OutputStream {
      override def write(b : Int) : Unit = Unit
  })

  def apply() : Task[Unit] = stream.merge.mergeN(maxOpen)(io.linesR("terms.txt").map(mine))
    .map(_.toString)
    .intersperse("\n")
    .pipe(text.utf8Encode)
    .to(io.fileChunkW(s"${System.currentTimeMillis}.csv"))
    .run

  private[this] def mine(term : String) : Process[Task, Row] = ids(term)
    .flatMap(id => article(id).flatMap(compileArticle))
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

    assert(seq.length === 1)

    seq(0)

  } }

  private[this] def ids(term : String) : Process[Task, String] = {

    def xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="${term}"&retmax=${retmax}&rettype=xml""")

    def lst = (xml \\ "eSearchResult" \\ "IdList" \\ "Id").map(_.text).toList

    Process.emitAll(lst).toSource

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

  private[this] def parse(sentence : String) : Tree[String] = {

    val errStream : PrintStream = System.err

    System.setErr(nullStream)

    val tree = (new Parser).apply(sentence)

    System.setErr(errStream)

    tree

  }

}
