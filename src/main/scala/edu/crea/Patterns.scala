package edu.crea

import scalaz._
import Scalaz._

package object Patterns {

  implicit class TreeAdditions[T](tree : Tree[T]) {

    def leaves : Stream[T] = tree match {
      case Tree.Node(terminal, Stream()) => Stream(terminal)
      case _ => tree.subForest.map(_.leaves).join
    }

  }

  sealed trait Term
  case class Atom(tree : Tree[String]) extends Term {

    def value : String = {

      tree.leaves.flatMap(_.split(" "))
        .map(terminal => Lemmatizer(terminal.toLowerCase))
        .mkString(" ")

    }

  }

  implicit val AtomEqual : Equal[Atom] = Equal.equal(_.value === _.value)

  class Preterminal(value : String) {

    def unapply(tree : Tree[String]) : Option[String] = tree match {
      case Tree.Node(value, Stream(Tree.Node(terminal, Stream()))) => terminal.some
      case _ => none
    }

  }

  object Preterminal {

    def apply(value : String) : Preterminal = new Preterminal(value)

  }

  val NP = "NP"
  val AtNP = "@NP"

  val NN = "NN"
  val NNS = "NNS"
  val NNP = "NNP"
  val NNPS = "NNPS"

  val preNN = Preterminal(NN)
  val preNNS = Preterminal(NNS)
  val preNNP = Preterminal(NNP)
  val preNNPS = Preterminal(NNPS)

  sealed trait ConstituentPattern

  object PredicateArgument extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Atom] = tree match {
      case preNN(_) | preNNS(_) | preNNP(_) | preNNPS(_) => Atom(tree).some
      case Tree.Node(NP, Stream(preNN(_), preNN(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNN(_), preNNS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNN(_), preNNP(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNN(_), preNNPS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNS(_), preNN(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNS(_), preNNS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNS(_), preNNP(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNS(_), preNNPS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNP(_), preNN(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNP(_), preNNS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNP(_), preNNP(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNP(_), preNNPS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNPS(_), preNN(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNPS(_), preNNS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNPS(_), preNNP(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(preNNPS(_), preNNPS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNN(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNNS(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNNP(_))) => Atom(tree).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNNPS(_))) => Atom(tree).some
      case _ => none
    }

    def test : Unit = {

      val id = "abc".leaf
      assert(PredicateArgument(NN.node(id)) === Atom(id).some)
      assert(PredicateArgument(NNS.node(id)) === Atom(id).some)
      assert(PredicateArgument(NNP.node(id)) === Atom(id).some)
      assert(PredicateArgument(NNPS.node(id)) === Atom(id).some)

      assert(PredicateArgument(NP.node(NN.node("car".leaf), NNS.node("monkeys".leaf))) === Atom("car monkey".leaf).some)
      assert(PredicateArgument(NP.node(NNS.node("monkeys".leaf), NN.node("car".leaf))) === Atom("monkeys car".leaf).some)
      assert(PredicateArgument(NP.node(NNS.node("monkeys".leaf), NNP.node("Toronto".leaf))) === Atom("monkeys Toronto".leaf).some)

    }

  }

}
