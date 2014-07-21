package edu.crea

import scalaz._
import Scalaz._

import Constituents._
import Terms._
import Trees._

package object Patterns {

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
    val prePRP = Preterminal(PRP)
    val prePRP$ = Preterminal(PRP$)
    val preCD = Preterminal(CD)
    val preEX = Preterminal(EX)

    def apply(tree : Tree[String]) : Option[Atom] = tree match {

      case preDT(_) | preRB(_) | preRBR(_) | preRBS(_) | preJJR(_) | preJJS(_) | prePRP(_) | prePRP$(_) | preEX(_) | preJJ(_) | preCD(_) =>

        Monoid[Atom].zero.some

      case preNN(_) | preNNS(_) | preNNP(_) | preNNPS(_)| preFW(_) =>

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

      case Tree.Node(PRN, Stream(Tree.Node(AtPRN, Stream(Tree.Node(LRB, Stream(_)), PredicateArgumentsExpression(arguments))), Tree.Node(RRB, Stream(_)))) =>

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

      case Tree.Node(ADJP, Stream(_, PredicateExpression(predicates))) =>

        predicates.some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(IN|TO|VBG, Stream(_)), PredicateExpression(predicates))) =>

        predicates.some

      case Tree.Node(S|AtS, Stream(PredicateExpression(predicates))) =>

        predicates.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Compound]] = apply(tree)

  }

  object PhraseExpression extends ConstituentPattern {

    def apply(tree :  Tree[String]) : Option[Tuple2[Stream[Atom], Stream[Compound]]] = tree match {

      case Tree.Node(NP|AtNP|S|AtS, Stream(PredicateArgumentsExpression(arguments), ClauseExpression(clauses))) =>

        (arguments, clauses).some

      case Tree.Node(NP|AtNP|S|AtS, Stream(PredicateArgumentsExpression(args1), PhraseExpression((args2, clauses)))) =>

        (args1 |+| args2, clauses).some

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

      case Tree.Node(AtS, Stream(Tree.Node(PP, Stream(Tree.Node(IN, Stream(_)), PhraseExpression((arguments, clauses)))))) =>

        (arguments, clauses).some

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

  }

}


