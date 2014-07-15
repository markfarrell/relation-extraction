package edu.crea

import scalaz._
import Scalaz._

package object Patterns {

  import Constituents._

  implicit def convertTree[T](javaTree : edu.berkeley.nlp.syntax.Tree[T]) : Tree[T] = {

    import scala.collection.JavaConverters._

    val children = javaTree.getChildren.asScala.toStream.map(convertTree)

    children.size match {
      case 0 => javaTree.getLabel.leaf
      case _ => Tree.node(javaTree.getLabel, children)
    }

  }

  sealed trait Term

  case class Atom(id : String) extends Term
  case class CompoundTerm[T <: Term](id : String, args : T) extends Term

  implicit val AtomEqual : Equal[Atom] = Equal.equal(_.id === _.id)

  implicit val MonovalentCompoundTermEqual : Equal[CompoundTerm[Atom]] = Equal.equal { (x, y) =>
    x.id === y.id &&
    x.args === y.args
  }

  implicit class TreeAdditions[T <% String](tree : Tree[T]) {

    def leaves : Stream[T] = tree match {
      case Tree.Node(terminal, Stream()) => Stream(terminal)
      case _ => tree.subForest.map(_.leaves).join
    }

    def id : String = {

      tree.leaves.flatMap(_.split(" "))
        .map(terminal => Lemmatizer(terminal.toLowerCase))
        .mkString(" ")

    }

  }
  class Preterminal(value : String) {

    def unapply(tree : Tree[String]) : Option[String] = tree match {
      case Tree.Node(value, Stream(Tree.Node(terminal, Stream()))) => terminal.some
      case _ => none
    }

  }

  object Preterminal {

    def apply(value : String) : Preterminal = new Preterminal(value)

  }

  sealed trait ConstituentPattern

  object PredicateArgument extends ConstituentPattern {

    val preNN = Preterminal(NN)
    val preNNS = Preterminal(NNS)
    val preNNP = Preterminal(NNP)
    val preNNPS = Preterminal(NNPS)
    val preDT = Preterminal(DT)

    def apply(tree : Tree[String]) : Option[Atom] = tree match {
      case preNN(_) | preNNS(_) | preNNP(_) | preNNPS(_) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNN(_), preNN(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNN(_), preNNS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNN(_), preNNP(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNN(_), preNNPS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNS(_), preNN(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNS(_), preNNS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNS(_), preNNP(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNS(_), preNNPS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNP(_), preNN(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNP(_), preNNS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNP(_), preNNP(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNP(_), preNNPS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNPS(_), preNN(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNPS(_), preNNS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNPS(_), preNNP(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(preNNPS(_), preNNPS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNN(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNNS(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNNP(_))) => Atom(tree.id).some
      case Tree.Node(NP, Stream(Tree.Node(AtNP, _), preNNPS(_))) => Atom(tree.id).some
      case Tree.Node(NP|AtNP, Stream(Tree.Node(DT, Stream()), x)) => Atom(x.id).some
      case _ => none
    }

    def unapply(tree : Tree[String]) : Option[Atom] = apply(tree)

    def test : Unit = {

      val id = "abc"
      assert(PredicateArgument(NN.node(id.leaf)) === Atom(id).some)
      assert(PredicateArgument(NNS.node(id.leaf)) === Atom(id).some)
      assert(PredicateArgument(NNP.node(id.leaf)) === Atom(id).some)
      assert(PredicateArgument(NNPS.node(id.leaf)) === Atom(id).some)

      assert(PredicateArgument(NP.node(NN.node("car".leaf), NNS.node("monkeys".leaf))) === Atom("car monkey").some)
      assert(PredicateArgument(NP.node(NNS.node("monkeys".leaf), NN.node("car".leaf))) === Atom("monkey car").some)
      assert(PredicateArgument(NP.node(NNS.node("monkeys".leaf), NNP.node("Toronto".leaf))) === Atom("monkey toronto").some)

    }

  }

  object MonovalentPredicateExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[CompoundTerm[_ <: Term]] = tree match {
      case Tree.Node(S|AtS, Stream(MonovalentPredicateExpression(term), _)) => term.some
      case Tree.Node(S|AtS, Stream(PredicateArgument(args), Tree.Node(VP, Stream(Tree.Node(_, Stream(l)))))) => CompoundTerm[Atom](l.id, args).some
      case _ => none
    }

    def unapply(tree : Tree[String]) : Option[CompoundTerm[_ <: Term]] = apply(tree)

    def test(implicit parse : MemoizedParser) : Unit = {

      assert(RootExpression(parse("The man sleeps.")) == CompoundTerm("sleep", Atom("man")).some)

    }

  }

  object RootExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Term] = tree match {
      case Tree.Node(ROOT, Stream(MonovalentPredicateExpression(term))) => term.some
      case _ => none
    }
  }

}

package object Constituents {

  val ROOT = "ROOT"

  val S = "S"
  val SBARQ = "SBARQ"
  val SBAR = "SBAR"
  val SINV = "SINV"
  val SQ = "SQ"
  val ADJP = "ADJP"
  val CONJP = "CONJP"
  val FRAG = "FRAG"
  val INTJ = "INTJ"
  val LST = "LST"
  val NAC = "NAC"
  val NP = "NP"
  val NX = "NX"
  val PP = "PP"
  val PRN = "PRN"
  val PRT = "PRT"
  val QP = "QP"
  val RRC = "RRC"
  val UCP = "UCP"
  val VP = "VP"
  val WHADJP = "WHADJP"
  val WHAVP = "WHAVP"
  val WHNP = "WHNP"
  val WHPP = "WHPP"
  val X = "X"
  val AtS = "@S"
  val AtSBARQ = "@SBARQ"
  val AtSBAR = "@SBAR"
  val AtSINV = "@SINV"
  val AtSQ = "@SQ"
  val AtADJP = "@ADJP"
  val AtCONJP = "@CONJP"
  val AtFRAG = "@FRAG"
  val AtINTJ = "@INTJ"
  val AtLST = "@LST"
  val AtNAC = "@NAC"
  val AtNP = "@NP"
  val AtNX = "@NX"
  val AtPP = "@PP"
  val AtPRN = "@PRN"
  val AtPRT = "@PRT"
  val AtQP = "@QP"
  val AtRRC = "@RRC"
  val AtUCP = "@UCP"
  val AtVP = "@VP"
  val AtWHADJP = "@WHADJP"
  val AtWHAVP = "@WHAVP"
  val AtWHNP = "@WHNP"
  val AtWHPP = "@WHPP"
  val AtX = "@X"
  val CC = "CC"
  val CD = "CD"
  val DT = "DT"
  val EX = "EX"
  val FW = "FW"
  val IN = "IN"
  val JJ = "JJ"
  val JJR = "JJR"
  val JJS = "JJS"
  val LS = "LS"
  val MD = "MD"
  val NN = "NN"
  val NNS = "NNS"
  val NNP = "NNP"
  val NNPS = "NNPS"
  val PDT = "PDT"
  val POS = "POS"
  val PRP = "PRP"
  val PRP$ = "PRP$"
  val RB = "RB"
  val RBR = "RBR"
  val RBS = "RBS"
  val RP = "RP"
  val SYM = "SYM"
  val TO = "TO"
  val UH = "UH"
  val VB = "VB"
  val VBD = "VBD"
  val VBG = "VBG"
  val VBN = "VBN"
  val VBP = "VBP"
  val VBZ = "VBZ"
  val WDT = "WDT"
  val WP = "WP"
  val WP$ = "WP$"
  val WRB = "WRB"

}
