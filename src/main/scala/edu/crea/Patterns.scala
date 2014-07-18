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

  case class Compound(atom : Atom = Monoid[Atom].zero, args : List[Atom] = Monoid[List[Atom]].zero) extends Term

  implicit val AtomEqual : Equal[Atom] = Equal.equal(_.id === _.id)

  implicit val CompoundEqual : Equal[Compound] = Equal.equal {
    (x, y) => (x.atom, x.args) === (y.atom, y.args)
  }

  implicit val AtomMonoid : Monoid[Atom] = new Monoid[Atom] {

    def zero : Atom = Atom(Monoid[String].zero)

    def append(a1 : Atom, a2: => Atom) : Atom = Atom(List(a1.id, a2.id).distinct.mkString(" ").trim)

  }

  implicit val CompoundMonoid : Monoid[Compound] = new Monoid[Compound] {

    def zero : Compound = Compound(Monoid[Atom].zero, Monoid[List[Atom]].zero)

    def append(a1 : Compound, a2 : => Compound) : Compound = {
      Compound(a1.atom |+| a2.atom, (a1.args |+| a2.args).distinct)
    }

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
        .trim
    }

  }

  class Preterminal(value : String) {

    def unapply(tree : Tree[String]) : Option[String] = tree match {
      case Tree.Node(compareValue, Stream(Tree.Node(terminal, Stream()))) if compareValue === value => terminal.some
      case _ => none
    }

  }

  object Preterminal {

    def apply(value : String) : Preterminal = new Preterminal(value)

  }

  sealed trait ConstituentPattern

  object PredicateArgumentExpression extends ConstituentPattern {

    val preNN = Preterminal(NN)
    val preNNS = Preterminal(NNS)
    val preNNP = Preterminal(NNP)
    val preNNPS = Preterminal(NNPS)
    val preDT = Preterminal(DT)

    def apply(tree : Tree[String]) : Option[Atom] = tree match {

      case preNN(_) | preNNS(_) | preNNP(_) | preNNPS(_) =>

        Atom(tree.id).some

      case Tree.Node(NP|AtNP, Stream(Tree.Node(DT, _), x)) =>

        Atom(x.id).some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentExpression(atom))) =>

        atom.some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentExpression(args1), PredicateArgumentExpression(args2))) =>

        args1.some |+| args2.some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentExpression(args1), Tree.Node(PP, Stream(Tree.Node(IN, Stream(_)), PredicateArgumentExpression(args2))))) =>

        args2.some |+| args1.some

      case Tree.Node(AtS, Stream(PredicateArgumentExpression(atom), Tree.Node(ADVP, Stream(Tree.Node(RB, Stream(_)))))) =>

        atom.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Atom] = apply(tree)

  }

  object PredicateArgumentsExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[List[Atom]] = tree match {

      case PredicateArgumentExpression(atom) =>

        List(atom).some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentsExpression(args1), PredicateArgumentsExpression(args2))) =>

        args1.some |+| args2.some

      case Tree.Node(S|AtS, Stream(PredicateArgumentsExpression(arguments), Tree.Node(_, Stream(_)))) =>

        arguments.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[List[Atom]] = apply(tree)

  }

  object PredicateExpression extends ConstituentPattern {

    private[this] val preVB = Preterminal(VB)
    private[this] val preVBD = Preterminal(VBD)
    private[this] val preVBG = Preterminal(VBG)
    private[this] val preVBN = Preterminal(VBN)
    private[this] val preVBP = Preterminal(VBP)
    private[this] val preVBZ = Preterminal(VBZ)

    def apply(tree : Tree[String]) : Option[List[Compound]] = tree match {

      case preVB(_) | preVBD(_) | preVBG(_) | preVBN(_) | preVBP(_) | preVBZ(_) =>

        List(Compound(atom=Atom(tree.id))).some

      case Tree.Node(VP|AtVP, Stream(preVBZ(_) | preVBD(_) | preVB(_) | preVBP(_), PredicateExpression(lst))) =>

        lst.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(lst1), PredicateExpression(lst2))) =>

        (lst1 |+| lst2).some

      case Tree.Node(VP|AtVP, Stream(_, PredicateExpression(lst))) =>

        lst.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(lst), Tree.Node(PRT, Stream(particle)))) =>

        lst.map((_ : Compound) |+| Compound(atom=Atom(particle.id))).some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PredicateArgumentsExpression(arguments))) =>

        predicates.map((_ : Compound) |+| Compound(args=arguments)).some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(lst))) =>

        lst.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(lst), Tree.Node(ADVP, Stream(Tree.Node(RB, Stream(_)))))) =>

        lst.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(lst), Tree.Node(_, Stream(_)))) =>

        lst.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[List[Compound]] = apply(tree)

  }

  object PhraseExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Tuple2[List[Atom], List[Compound]]] = tree match {

      case Tree.Node(NP|AtNP|S|AtS, Stream(PredicateArgumentsExpression(arguments), ClauseExpression(clauses))) =>

        (arguments, clauses).some

      case Tree.Node(NP|AtNP|S|AtS, Stream(ClauseExpression(clauses), PredicateArgumentsExpression(arguments))) =>

        (arguments, clauses).some

      case Tree.Node(S|AtS, Stream(PhraseExpression((arguments, clauses)), Tree.Node(_, Stream(_)))) =>

        (arguments, clauses).some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Tuple2[List[Atom], List[Compound]]] = apply(tree)

  }

  object ClauseExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[List[Compound]] = tree match {

      case Tree.Node(S|AtS|SBAR|AtSBAR, Stream(ClauseExpression(clauses1), ClauseExpression(clauses2))) =>

        clauses1.some |+| clauses2.some

      case Tree.Node(S|AtS|SBAR|AtSBAR, Stream(ClauseExpression(clauses), Tree.Node(_, Stream(_)))) =>

        clauses.some

      case Tree.Node(S|AtS, Stream(PhraseExpression((arguments, clauses)), PredicateExpression(predicates))) => {

        clauses.some |+| predicates.map(compound => Compound(args=arguments) |+| compound).some

      }

      case Tree.Node(S|AtS, Stream(PredicateArgumentsExpression(arguments), PredicateExpression(predicates))) =>

        predicates.map(compound => Compound(args=arguments) |+| compound).some

      case Tree.Node(AtSBAR, Stream(ClauseExpression(clauses), Tree.Node(_, Stream(_)))) =>

        clauses.some

      case Tree.Node(SBAR, Stream(Tree.Node(IN, Stream(_)), ClauseExpression(clauses))) =>

        clauses.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[List[Compound]] = apply(tree)

  }

  object RootExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[List[Compound]] = tree match {
      case Tree.Node(ROOT, Stream(ClauseExpression(lst))) => lst.some
      case _ => none
    }

    def test(implicit parse : MemoizedParser) : Unit = {

      implicit def string2Result(str : String) : Option[List[Compound]] = RootExpression(parse(str))

      {
        val expect = List(Compound(Atom("walk"),List(Atom("toronto monkey")))).some
        assert(expect === "The Toronto monkey can walk.")
        assert(expect === "The Toronto monkeys can walk.")
        assert(expect === "The Toronto Monkeys can walk.")
      }

      {
        val expect = List(Compound(Atom("sleep"), List(Atom("man")))).some
        assert(expect === "The man sleeps.")
        assert(expect === "The man is sleeping.")
        assert(expect === "The man was freely sleeping.")
        assert(expect === "The man freely sleeps.")
        assert(expect === "The man freely has slept.")
        assert(expect === "The man might sleep.")
        assert(expect === "The man might be sleeping.")
        assert(expect === "The man might sleep quietly.")
        assert(expect === "The man might quietly and patiently sleep.")
      }

      {
        val expect = List(Compound(Atom("take off"), List(Atom("man")))).some
        assert(expect === "The man takes off.")
        assert(expect === "The man has taken off.")
        assert(expect === "The man might take off.")
        assert(expect === "The man might have taken off.")
        assert(expect === "The man might freely have taken off.")
      }

      {
        val expect = List(Compound(Atom("take"),List(Atom("man type"), Atom("dog")))).some
        assert(expect === "That type of man took the dog.")
        assert(expect === "That type of man has taken the dog.")
      }

      {
        val expect = List(Compound(Atom("walk"),List(Atom("man"), Atom("dog"))), Compound(Atom("eat"),List(Atom("man")))).some
        assert(expect === "The man can walk the dog and can eat.")
      }

      {
        val expect = List(Compound(Atom("see"),List(Atom("man"))), Compound(Atom("walk"),List(Atom("man"), Atom("dog")))).some
        assert(expect === "The man if the man can see walked the dog.")
        assert(expect === "The man, if the man can see, can walk the dog.")
      }

      {
        val expect = List(Compound(Atom("see"),List(Atom("man"))), Compound(Atom("walk"),List(Atom("man"))), Compound(Atom("walk"),List(Atom("man"), Atom("dog")))).some
        assert(expect === "The man, if the man can see, and if the man can walk, can walk the dog.")
      }

      {
        val expect = List(Compound(Atom("walk"),List(Atom("man"))), Compound(Atom("walk"),List(Atom("dog")))).some
        assert(expect === "If the man can walk the dog can walk.")
        assert(expect === "If the man can walk, the dog can walk.")
        assert(expect === "If the man can walk, then the dog can walk.")
      }
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
  val ADVP = "ADVP"
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
  val AtADVP = "@ADVP"
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

object ToGraph {

  import Patterns._

  import org.gephi.graph.api.{GraphModel, Node, Edge}

  def apply(clauses : List[Compound], model : GraphModel) : Unit = {

    for {
      compound <- clauses
      (sourceAtom, targetAtom) <- compound.args.sliding(2).map(lst => (lst.head, lst.last))
    } {

      val label = compound.atom.id
      val source = createNode(sourceAtom.id, model)
      val target = createNode(targetAtom.id, model)

      createEdge(label, source, target, model)

    }

  }

  private[this] def createNode(label : String, model : GraphModel) : Node = {

    Option(model.getGraph.getNode(label)) match {
      case Some(node) => node
      case None => {

        val node = model.factory.newNode(label)

        node.getNodeData.setLabel(label)
        node.getNodeData.setColor(0.5f, 0.5f, 0.5f)

        model.getGraph.addNode(node)

        node
      }
    }

  }

  private[this] def createEdge(label : String, source : Node, target : Node, model : GraphModel) : Edge = {

    val edge = model.factory.newEdge(source, target)

    edge.getEdgeData.setColor(0.5f, 0.5f, 0.5f)
    edge.getEdgeData.setLabel(label)

    model.getGraph.addEdge(edge)

    edge

  }

}
