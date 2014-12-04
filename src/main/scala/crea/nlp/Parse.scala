package crea.nlp

import scalaz._
import Scalaz._

object Parse {

  private[this] lazy val parser = new Parser

  def apply(sentence : String) : Tree[String] = this.synchronized { 
    parser.apply(sentence)
  }

}
