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

  private[this] val retmax = 10000

  def apply(term : String) : Process[Task, Relation] = ids(term)
    .flatMap(id => article(id).flatMap(compileArticle))
    .flatMap(lst => Process.emitAll(lst))
    .filter(_.args.length === 2)

  private[this] def article(id : Int) : Process[Task, Pubmed] = Process.eval { Task {

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

  private[this] def ids(term : String) : Process[Task, Int] = {

    def xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="${term}"&retmax=${retmax}&rettype=xml""")

    def lst = (xml \\ "eSearchResult" \\ "IdList" \\ "Id").map(_.text.toInt).toList

    Process.emitAll(lst).toSource

  }

  private[this] def compileArticle(article : Pubmed) : Process[Task, List[Relation]] = Process.emitAll(article._abstract).flatMap(compileSentence)

  private[this] def compileSentence(sentence : String) : Process[Task, List[Relation]] = Process.eval { Task {

    Compile(parse(sentence)).toList.flatten

  }.timed(10000).or(Task.now(List.empty[Relation])) }

  private[this] def parse(sentence : String) : Tree[String] = {

    val errStream : PrintStream = System.err

    System.setErr(new PrintStream(new OutputStream {
      override def write(b : Int) : Unit = Unit
    }))

    val tree = (new Parser).apply(sentence)

    System.setErr(errStream)

    tree

  }

}
