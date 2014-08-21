package crea.nlp

import scala.xml.XML

final case class Pubmed(pmid : String, title : String, _abstract : String)

object Pubmed {

  def apply(path : String) : String = articles(path).map(_._abstract).mkString("\n")

  def articles(path : String) : Seq[Pubmed] = {

    val xml = XML.loadFile(path)

    (xml \\ "PubmedArticleSet" \\ "PubmedArticle").map { article =>

      val pmid = (article \\ "PMID").text
      val title = (article \\ "ArticleTitle").text
      val _abstract = (article \\ "Abstract").text

      Pubmed(pmid, title, _abstract)

    }

  }

}


