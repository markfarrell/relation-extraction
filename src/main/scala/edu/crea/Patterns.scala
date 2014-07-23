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

    private[this] val preNN = Preterminal(NN)
    private[this] val preNNS = Preterminal(NNS)
    private[this] val preNNP = Preterminal(NNP)
    private[this] val preNNPS = Preterminal(NNPS)
    private[this] val preDT = Preterminal(DT)
    private[this] val preJJ = Preterminal(JJ)
    private[this] val preJJR = Preterminal(JJR)
    private[this] val preJJS = Preterminal(JJS)
    private[this] val preRB = Preterminal(RB)
    private[this] val preRBR = Preterminal(RBR)
    private[this] val preRBS = Preterminal(RBS)
    private[this] val preFW = Preterminal(FW)
    private[this] val prePRP = Preterminal(PRP)
    private[this] val prePRP$ = Preterminal(PRP$)
    private[this] val preCD = Preterminal(CD)
    private[this] val preEX = Preterminal(EX)

    def apply(tree : Tree[String]) : Option[Atom] = tree match {

      case preDT(_) | preRB(_) | preRBR(_) | preRBS(_) | preJJR(_) | preJJS(_) | prePRP(_) | prePRP$(_) | preEX(_) | preJJ(_) | preCD(_) =>

        Monoid[Atom].zero.some

      case preNN(_) | preNNS(_) | preNNP(_) | preNNPS(_)| preFW(_) =>

        Atom(tree.id).some

      case Tree.Node(NP|AtNP, Stream(Tree.Node(VBG|VBN, Stream(x)), PredicateArgumentExpression(arg))) =>

        Atom(x.id).some |+| arg.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(arg))) =>

        arg.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(Tree.Node(DT, Stream(_)), PredicateArgumentExpression(arg))) =>

        arg.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(arg1), PredicateArgumentExpression(arg2))) =>

        arg1.some |+| arg2.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(arg), Tree.Node(_, Stream(_)))) =>
        arg.some

        case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Atom] = apply(tree)

  }

  object PredicateArgumentsExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Stream[Atom]] = tree match {

      case Tree.Node(NP|AtNP, Stream(
        Tree.Node(AtNP, Stream(
          PredicateArgumentsExpression(args1),
          Tree.Node(CC|COMMA, Stream(_))
        )),
        PredicateArgumentsExpression(args2)
      )) =>

        args1.some |+| args2.some

      case PredicateArgumentExpression(arg) =>

        Stream(arg).some


      case Tree.Node(AtS, Stream(PredicateArgumentsExpression(args), Tree.Node(ADVP, Stream(Tree.Node(RB, Stream(_)))))) =>

        args.some

      case Tree.Node(S|AtS, Stream(PredicateArgumentsExpression(arguments), Tree.Node(_, Stream(_)))) =>

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

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), Tree.Node(PRT, Stream(particle)))) =>

        predicates.map((_ : Compound) |+| Compound(atom=Atom(particle.id))).some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PredicateArgumentsExpression(arguments))) =>

        def newPredicates = predicates.flatMap { compound =>

          arguments.map(arg =>Compound(args=Stream(arg)) |+| compound)

        }

        newPredicates.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream), Tree.Node(ADVP, Stream(Tree.Node(RB, Stream(_)))))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream), Tree.Node(_, Stream(_)))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PrepositionalPhraseExpression((arguments, clauses)))) =>

        def newPredicates = predicates.flatMap { compound =>

          arguments.map(arg =>Compound(args=Stream(arg)) |+| compound)

        }

        newPredicates.some |+| clauses.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PhraseExpression((arguments, clauses)))) =>

        def newPredicates = predicates.flatMap { compound =>

          arguments.map(arg =>Compound(args=Stream(arg)) |+| compound)

        }

        newPredicates.some |+| clauses.some

      case Tree.Node(ADJP, Stream(_, PredicateExpression(predicates))) =>

        predicates.some

      case Tree.Node(S|AtS, Stream(PredicateExpression(predicates))) =>

        predicates.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Compound]] = apply(tree)

  }

  object PrepositionalPhraseExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Tuple2[Stream[Atom], Stream[Compound]]] = tree match {

      case Tree.Node(PP|AtPP, Stream(Tree.Node(IN|VBG|TO, Stream(_)), PhraseExpression((arguments, clauses)))) =>

        (arguments, clauses).some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(IN|VBG|TO, Stream(_)), PredicateArgumentsExpression(arguments))) =>

        (arguments, Stream()).some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(AtPP, Stream(_, Tree.Node(IN|VBG|TO, Stream(_)))), PhraseExpression((arguments, clauses)))) =>

        (arguments, clauses).some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(AtPP, Stream(_, Tree.Node(IN|VBG|TO, Stream(_)))), PredicateArgumentsExpression(arguments))) =>

        (arguments, Stream()).some

      case Tree.Node(PRN, Stream(
        Tree.Node(AtPRN, Stream(
          Tree.Node(LRB, Stream(_)),
          PrepositionalPhraseExpression((arguments, clauses))
        )),
        Tree.Node(RRB, Stream(_))
      )) =>

        (arguments, clauses).some

      case _ => none

    }

    def unapply(tree : Tree[String]) :  Option[Tuple2[Stream[Atom], Stream[Compound]]] = apply(tree)

  }

  object PhraseExpression extends ConstituentPattern {

    def apply(tree :  Tree[String]) : Option[Tuple2[Stream[Atom], Stream[Compound]]] = tree match {

      case Tree.Node(NP|AtNP|S|AtS, Stream(PredicateArgumentsExpression(args1), PrepositionalPhraseExpression((args2, clauses)))) =>

        def arguments = args2.flatMap(arg2 => args1.map(arg1 => arg2 |+| arg1))

        (arguments, clauses).some

      case Tree.Node(PRN, Stream(
        Tree.Node(AtPRN, Stream(
          Tree.Node(LRB, Stream(_)),
          PhraseExpression((arguments, clauses))
        )),
        Tree.Node(RRB, Stream(_))
      )) =>

        (arguments, clauses).some

      case Tree.Node(PRN, Stream(
        Tree.Node(AtPRN, Stream(
          Tree.Node(LRB, Stream(_)),
          PredicateArgumentsExpression(arguments)
        )),
        Tree.Node(RRB, Stream(_))
      )) =>

        (arguments, Stream()).some

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

        def newPredicates = predicates.flatMap { compound =>

          arguments.map(arg =>Compound(args=Stream(arg)) |+| compound)

        }

        (arguments, newPredicates).some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentsExpression(arguments), PredicateExpression(predicates))) =>

        def newPredicates = predicates.flatMap { compound =>

          arguments.map(arg =>Compound(args=Stream(arg)) |+| compound)

        }

        (arguments, newPredicates).some

      case Tree.Node(S|AtS, Stream(PhraseExpression((arguments, clauses)))) =>

        (arguments, clauses).some

      case Tree.Node(S|AtS, Stream(PhraseExpression((args1, clauses1)), PhraseExpression(args2, clauses2))) =>

        (args1 |+| args2, clauses1 |+| clauses2).some

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

        clauses.some |+| predicates.flatMap { compound =>

          arguments.map(arg =>Compound(args=Stream(arg)) |+| compound)

        }.some

      case Tree.Node(S|AtS, Stream(PredicateArgumentsExpression(arguments), PredicateExpression(predicates))) =>

        predicates.flatMap { compound =>

          arguments.map(arg =>Compound(args=Stream(arg)) |+| compound)

        }.some

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


