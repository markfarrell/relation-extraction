package crea.nlp

import scalaz._
import Scalaz._

import Patterns._
import Terms._

object Compile {

  def apply(tree : Tree[String]) : Tree[String] \/ List[Relation] = RootExpression(tree) match {
    case Some(compounds) => compounds.toList.right
    case None => tree.left
  }

}
