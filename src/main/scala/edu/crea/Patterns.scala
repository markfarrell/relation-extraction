package edu.crea

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import scalaz._
import Scalaz._

package object Patterns {

  import Constituents._

  import Isomorphism.<=>

  type BerkeleyTree[T] = edu.berkeley.nlp.syntax.Tree[T]

  implicit def isoTree[T] = new (BerkeleyTree[T] <=> Tree[T]) {

    val to : BerkeleyTree[T] => Tree[T] = { berkeleyTree =>

      val children = berkeleyTree.getChildren.asScala.toStream.map(to)

      children.size match {
        case 0 => berkeleyTree.getLabel.leaf
        case _ => Tree.node(berkeleyTree.getLabel, children)
      }

    }

    val from : Tree[T] => BerkeleyTree[T] = {
       case Tree.Node(label, Stream()) => new BerkeleyTree[T](label)
       case Tree.Node(label, subForest) => new BerkeleyTree[T](label, subForest.map(from).toList)
    }

  }

  implicit def toTree[T](berkeleyTree : BerkeleyTree[T]) : Tree[T] = isoTree.to(berkeleyTree)
  implicit def fromTree[T](tree : Tree[T]) : BerkeleyTree[T] = isoTree.from(tree)

  implicit def showTree[T] = new Show[Tree[T]] {

    override def shows(tree : Tree[T]) : String = {

      import edu.berkeley.nlp.syntax.Trees.PennTreeRenderer

      PennTreeRenderer.render(tree)

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

  sealed trait Term

  case class Atom(id : String) extends Term

  case class Compound(atom : Atom = Monoid[Atom].zero, args : Stream[Atom] = Monoid[Stream[Atom]].zero) extends Term

  implicit val equalAtom : Equal[Atom] = Equal.equal(_.id === _.id)

  implicit val equalCompound : Equal[Compound] = Equal.equal {
    (x, y) => (x.atom, x.args) === (y.atom, y.args)
  }

  implicit val monoidAtom : Monoid[Atom] = new Monoid[Atom] {

    def zero : Atom = Atom(Monoid[String].zero)

    def append(a1 : Atom, a2: => Atom) : Atom = Atom(Stream(a1.id, a2.id).distinct.mkString(" ").trim)

  }

  implicit val monoidCompound : Monoid[Compound] = new Monoid[Compound] {

    def zero : Compound = Compound(Monoid[Atom].zero, Monoid[Stream[Atom]].zero)

    def append(a1 : Compound, a2 : => Compound) : Compound = {
      Compound(a1.atom |+| a2.atom, (a1.args |+| a2.args).filterNot(_ === Monoid[Atom].zero).distinct)
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
    val preJJ = Preterminal(JJ)
    val preJJR = Preterminal(JJR)
    val preJJS = Preterminal(JJS)
    val preRB = Preterminal(RB)
    val preRBR = Preterminal(RBR)
    val preRBS = Preterminal(RBS)
    val preFW = Preterminal(FW)

    def apply(tree : Tree[String]) : Option[Atom] = tree match {

      case preDT(_) | preRB(_) | preRBR(_) | preRBS(_) | preJJR(_) | preJJS(_) =>

        Monoid[Atom].zero.some

      case preNN(_) | preNNS(_) | preNNP(_) | preNNPS(_) | preJJ(_) | preFW(_) =>

        Atom(tree.id).some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(atom))) =>

        atom.some

      case Tree.Node(NP|AtNP, Stream(Tree.Node(DT, Stream(_)), PredicateArgumentExpression(atom))) =>

        atom.some

      case Tree.Node(NP|AtNP, Stream(Tree.Node(VBG|VBN, Stream(x)), PredicateArgumentExpression(atom))) =>

        Atom(x.id).some |+| atom.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(atom))) =>

        atom.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(args1), PredicateArgumentExpression(args2))) =>

        args1.some |+| args2.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(atom), Tree.Node(_, Stream(_)))) =>

        atom.some


      case Tree.Node(AtS, Stream(PredicateArgumentExpression(atom), Tree.Node(ADVP, Stream(Tree.Node(RB, Stream(_)))))) =>

        atom.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Atom] = apply(tree)

  }

  object PredicateArgumentsExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Stream[Atom]] = tree match {

      case PredicateArgumentExpression(atom) =>

        Stream(atom).some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentExpression(argument), Tree.Node(PP, Stream(Tree.Node(IN|VBG|TO, Stream(_)), PredicateArgumentsExpression(args2))))) =>

        args2.map((_ : Atom) |+| argument).some

      case Tree.Node(NP|AtNP|ADJP|AtADJP|PP|AtPP, Stream(PredicateArgumentsExpression(args1), PredicateArgumentsExpression(args2))) =>

        args1.some |+| args2.some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(IN|VBG|TO, Stream(_)), PredicateArgumentsExpression(arguments))) =>

        arguments.some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(AtPP, Stream(_, Tree.Node(IN|VBG|TO, Stream(_)))), PredicateArgumentsExpression(arguments))) =>

        arguments.some

      case Tree.Node(ADJP|AtADJP, Stream(Tree.Node(JJ, Stream(_)), PredicateArgumentsExpression(arguments))) =>

        arguments.some

      case Tree.Node(S|AtS|NP|AtNP|PP|AtPP, Stream(PredicateArgumentsExpression(arguments), Tree.Node(_, Stream(_)))) =>

        arguments.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Atom]] = apply(tree)

  }

  object PredicateExpression extends ConstituentPattern {

    private[this] val preVB = Preterminal(VB)
    private[this] val preVBD = Preterminal(VBD)
    private[this] val preVBG = Preterminal(VBG)
    private[this] val preVBN = Preterminal(VBN)
    private[this] val preVBP = Preterminal(VBP)
    private[this] val preVBZ = Preterminal(VBZ)

    def apply(tree : Tree[String]) : Option[Stream[Compound]] = tree match {

      case preVB(_) | preVBD(_) | preVBG(_) | preVBN(_) | preVBP(_) | preVBZ(_) =>

        Stream(Compound(atom=Atom(tree.id))).some

      case Tree.Node(VP|AtVP, Stream(preVB(_) | preVBD(_) | preVBG(_) | preVBN(_) | preVBP(_) | preVBZ(_),  PredicateExpression(stream))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream1), PredicateExpression(stream2))) =>

        (stream1 |+| stream2).some

      case Tree.Node(VP|AtVP, Stream(_, PredicateExpression(stream))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream), Tree.Node(PRT, Stream(particle)))) =>

        stream.map((_ : Compound) |+| Compound(atom=Atom(particle.id))).some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PredicateArgumentsExpression(arguments))) =>

        predicates.map((_ : Compound) |+| Compound(args=arguments)).some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream), Tree.Node(ADVP, Stream(Tree.Node(RB, Stream(_)))))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream), Tree.Node(_, Stream(_)))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PhraseExpression((arguments, clauses)))) =>

        predicates.map(compound => Compound(args=arguments) |+| compound).some |+| clauses.some

      case Tree.Node(ADJP, Stream(_,
        Tree.Node(PP, Stream(
          Tree.Node(IN, Stream(_)),
          Tree.Node(S, Stream(PredicateExpression(predicates)))
        ))
      )) =>

        predicates.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Compound]] = apply(tree)

  }

  object PhraseExpression extends ConstituentPattern {

    def apply(tree :  Tree[String]) : Option[Tuple2[Stream[Atom], Stream[Compound]]] = tree match {

      case Tree.Node(NP|AtNP|S|AtS, Stream(PredicateArgumentsExpression(arguments), ClauseExpression(clauses))) =>

        (arguments, clauses).some

      case Tree.Node(NP|AtNP|S|AtS, Stream(ClauseExpression(clauses), PredicateArgumentsExpression(arguments))) =>

        (arguments, clauses).some

      case Tree.Node(NP|AtNP|S|AtS, Stream(PhraseExpression((arguments, clauses)), Tree.Node(_, Stream(_)))) =>

        (arguments, clauses).some

      case Tree.Node(NP|AtNP, Stream(
        PredicateArgumentsExpression(arguments),
        Tree.Node(SBAR, Stream(
          Tree.Node(WHNP, Stream(
            Tree.Node(_, Stream(_)))),
            Tree.Node(S|AtS, Stream(
              PredicateExpression(predicates)
            ))
        ))
      )) =>

        (arguments, predicates.map(compound => Compound(args=arguments) |+| compound)).some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentsExpression(arguments), PredicateExpression(predicates))) =>

        (arguments, predicates.map(compound => Compound(args=arguments) |+| compound)).some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Tuple2[Stream[Atom], Stream[Compound]]] = apply(tree)

  }

  object ClauseExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Stream[Compound]] = tree match {

      case Tree.Node(S|AtS|SBAR|AtSBAR, Stream(ClauseExpression(clauses1), ClauseExpression(clauses2))) =>

        clauses1.some |+| clauses2.some

      case Tree.Node(S|AtS|SBAR|AtSBAR, Stream(ClauseExpression(clauses), Tree.Node(_, Stream(_)))) =>

        clauses.some

      case Tree.Node(S|AtS, Stream(PhraseExpression((arguments, clauses)), PredicateExpression(predicates))) =>

        clauses.some |+| predicates.map(compound => Compound(args=arguments) |+| compound).some

      case Tree.Node(S|AtS, Stream(PredicateArgumentsExpression(arguments), PredicateExpression(predicates))) =>

        predicates.map(compound => Compound(args=arguments) |+| compound).some

      case Tree.Node(AtSBAR, Stream(ClauseExpression(clauses), Tree.Node(_, Stream(_)))) =>

        clauses.some

      case Tree.Node(SBAR, Stream(Tree.Node(IN, Stream(_)), ClauseExpression(clauses))) =>

        clauses.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Compound]] = apply(tree)

  }

  object RootExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Stream[Compound]] = tree match {
      case Tree.Node(ROOT, Stream(ClauseExpression(stream))) => stream.some
      case _ => none
    }

    def test : Unit = {

      val parse = new MemoizedParser

      implicit def string2Result(str : String) : Option[Stream[Compound]] = RootExpression(parse(str))

      {
        val expect = Stream(Compound(Atom("walk"),Stream(Atom("toronto monkey")))).some
        assert(expect === "The Toronto monkey can walk.")
        assert(expect === "The Toronto monkeys can walk.")
        assert(expect === "The Toronto Monkeys can walk.")
      }

      {
        val expect = Stream(Compound(Atom("sleep"), Stream(Atom("man")))).some
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
        val expect = Stream(Compound(Atom("take off"), Stream(Atom("man")))).some
        assert(expect === "The man takes off.")
        assert(expect === "The man has taken off.")
        assert(expect === "The man might take off.")
        assert(expect === "The man might have taken off.")
        assert(expect === "The man might freely have taken off.")
      }

      {
        val expect = Stream(Compound(Atom("take"),Stream(Atom("man type"), Atom("dog")))).some
        assert(expect === "That type of man took the dog.")
        assert(expect === "That type of man has taken the dog.")
      }

      {
        val expect = Stream(Compound(Atom("walk"),Stream(Atom("man"), Atom("dog"))), Compound(Atom("eat"),Stream(Atom("man")))).some
        assert(expect === "The man can walk the dog and can eat.")
      }

      {
        val expect = Stream(Compound(Atom("see"),Stream(Atom("man"))), Compound(Atom("walk"),Stream(Atom("man"), Atom("dog")))).some
        assert(expect === "The man if the man can see walked the dog.")
        assert(expect === "The man, if the man can see, can walk the dog.")
      }

      {
        val expect = Stream(Compound(Atom("see"), Stream(Atom("man"))), Compound(Atom("walk"), Stream(Atom("man"))), Compound(Atom("walk"), Stream(Atom("man"), Atom("dog")))).some
        assert(expect === "The man, if the man can see, and if the man can walk, can walk the dog.")
      }

      {
        val expect = Stream(Compound(Atom("walk"), Stream(Atom("man"))), Compound(Atom("walk"), Stream(Atom("dog")))).some
        assert(expect === "If the man can walk the dog can walk.")
        assert(expect === "If the man can walk, the dog can walk.")
        assert(expect === "If the man can walk, then the dog can walk.")
      }

      {
        val expect = Stream(Compound(Atom("depend"), Stream(Atom("age disease progression"), Atom("critical crucial health balance")))).some
        assert(expect === "Age disease progression has depended on the critical and crucial health balance.")
        assert(expect === "Age disease progression depends on the critical and crucial health balance.")
        assert(expect === "Disease progression with aging undoubtedly depends on the critical and crucial health balance.")
        assert(expect === "Disease progression with aging depends undoubtedly on the critical and crucial health balance.")
        assert(expect === "Age disease progression has depended on the highly critical, crucial health balance.")
        assert(expect === "Age disease progression has depended on the critical and highly crucial health balance.")
        assert(expect === "Age disease progression has depended on the critical, highly crucial health balance.")
        assert(expect === "Age disease progression also depends on the critical, highly crucial health balance.")
      }

      {
        val expect = Stream(
          Compound(Atom("worsen"), Stream(Atom("age disease progression"))),
          Compound(Atom("depend"), Stream(Atom("age disease progression"), Atom("critical crucial health balance")))
        ).some
        assert(expect === "Age disease progression that can worsen also depends on the critical, highly crucial health balance.")
      }

      {
        val expect = Stream(
          Compound(Atom("be"), Stream(Atom("age disease progression"), Atom("unfortunate"))),
          Compound(Atom("depend"), Stream(Atom("age disease progression"), Atom("critical crucial health balance")))
        ).some
        assert(expect === "Age disease progression, which is unfortunate, also depends on the critical, highly crucial health balance.")
        assert(expect === "Age disease progression, which is highly unfortunate, also depends on the critical, highly crucial health balance.")
      }
    }

  }

}

object ToGraph {

  import Patterns._

  import org.gephi.graph.api.{GraphModel, Node, Edge}

  def apply(clauses : Stream[Compound], model : GraphModel) : Unit = {

    for {
      compound <- clauses
      (sourceAtom, targetAtom) <- compound.args.sliding(2).map(stream => (stream.head, stream.last))
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
