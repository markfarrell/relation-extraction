package crea.nlp

import scala.xml.XML

final case class Pubmed(pmid : String, title : String, _abstract : String)

object Pubmed {

  def apply(term : String) : Stream[Pubmed] = ids(term).flatMap { id =>

    val xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=${id}&rettype=xml""")

    (xml \\ "PubmedArticleSet" \\ "PubmedArticle").map { article =>

      val pmid = (article \\ "PMID").text
      val title = (article \\ "ArticleTitle").text
      val _abstract = (article \\ "Abstract").text

      Pubmed(pmid, title, _abstract)

    }

  }

  def ids(term : String) : Stream[Int] = {

    val xml = XML.load(s"""http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="${term}"&retmax=2000&rettype=xml""")

    (xml \\ "eSearchResult" \\ "IdList" \\ "Id").map(_.text.toInt).toStream

  }

}

object PubmedExtract {

  import scalaz._
  import Scalaz._
  import Terms._

  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import java.io.{OutputStream, PrintStream}

  def apply(term : String) : Stream[Future[Tree[String] \/ List[Relation]]] = {

    Pubmed(term).flatMap(x => Tokenize(x._abstract))
      .map(y => future { Compile(Parse(y)) })

  }

}
