package crea.nlp

import scalaz._
import Scalaz._

object Parse {

  lazy val parser = new Parser

  def apply(sentence : String) : Tree[String] = parser.apply(sentence)

}
