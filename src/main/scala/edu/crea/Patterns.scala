package edu.crea

import scalaz._
import Scalaz._

package object Patterns {

  sealed trait Term
  case class Atom(identifier : String) extends Term

  case class Label(value : String)
  val NN = Label("NN")

  implicit val AtomEqual : Equal[Atom] = Equal.equal(_.identifier == _.identifier)

  sealed trait ConstituentPattern

  class Preterminal(pre : Label) {

    def unapply(tree : Tree[Label]) : Option[Label] = tree match {
      case Tree.Node(pre, Stream(Tree.Node(label, Stream()))) => label.some
      case _ => none
    }

  }

  object Preterminal {

    def apply(pre : Label) : Preterminal = new Preterminal(pre)
  }

  object PredicateArgument extends ConstituentPattern {

    val preNN = Preterminal(NN)

    def apply(tree : Tree[Label]) : Option[Atom] = tree match {
      case preNN(label) => Atom(label.value).some
      case _ => none
    }

    def test : Unit = {
      val id = "abc"
      assert(PredicateArgument(NN.node(Label(id).leaf)) === Atom(id).some)
    }

  }

}
