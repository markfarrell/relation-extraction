package crea.nlp

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

  private[this] def applyArguments(arguments : => Stream[Literal])(relation : Relation) : Stream[Relation] = {

    val lit = relation.literal
    val args = relation.args.takeRight(1)

    arguments.map { arg => 

      val newArgs = (arg :: args).filterNot(_ === Monoid[Literal].zero)

      Relation(lit, newArgs)

    } 

  }

  sealed trait ConstituentPattern

  object PredicateArgumentExpression extends ConstituentPattern {

    private[this] val preNN = new Preterminal(NN)
    private[this] val preNNS = new Preterminal(NNS)
    private[this] val preNNP = new Preterminal(NNP)
    private[this] val preNNPS = new Preterminal(NNPS)
    private[this] val preDT = new Preterminal(DT)
    private[this] val preJJ = new Preterminal(JJ)
    private[this] val preJJR = new Preterminal(JJR)
    private[this] val preJJS = new Preterminal(JJS)
    private[this] val preRB = new Preterminal(RB)
    private[this] val preRBR = new Preterminal(RBR)
    private[this] val preRBS = new Preterminal(RBS)
    private[this] val preFW = new Preterminal(FW)
    private[this] val prePRP = new Preterminal(PRP)
    private[this] val prePRP$ = new Preterminal(PRP$)
    private[this] val preCD = new Preterminal(CD)
    private[this] val preEX = new Preterminal(EX)

    def apply(tree : Tree[String]) : Option[Literal] = tree match {

      case Tree.Node(ADVP|AtADVP, _) =>

        Monoid[Literal].zero.some

      case preDT(_) | preRB(_) | preRBR(_) | preRBS(_) | preJJR(_) | preJJS(_)
        | prePRP(_) | prePRP$(_) | preEX(_) | preJJ(_) =>

        Monoid[Literal].zero.some

      case preNN(_) | preNNS(_) | preNNP(_) | preNNPS(_)| preFW(_) | preCD(_) =>

        Literal(tree.id).some

      case Tree.Node(NP|AtNP, Stream(Tree.Node(VBG|VBN, Stream(x)), PredicateArgumentExpression(arg))) =>

        Literal(x.id).some |+| arg.some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentExpression(arg), Tree.Node(VBG|VBN, Stream(x)))) =>

        arg.some |+| Literal(x.id).some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(arg))) =>

        arg.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(Tree.Node(DT, Stream(_)), PredicateArgumentExpression(arg))) =>

        arg.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(arg1), PredicateArgumentExpression(arg2))) =>

        arg1.some |+| arg2.some

      case Tree.Node(NP|AtNP|ADJP|AtADJP, Stream(PredicateArgumentExpression(arg), Tree.Node(CC|COMMA, Stream(_)))) =>

        arg.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Literal] = apply(tree)

  }

  object PredicateArgumentsExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Stream[Literal]] = tree match {

      case Tree.Node(NP|AtNP, Stream(
        Tree.Node(AtNP, Stream(
          PredicateArgumentsExpression(args1),
          Tree.Node(CC|COMMA|CONJP|COLON, _)
        )),
        PredicateArgumentsExpression(args2)
      )) =>

        args1.some |+| args2.some

      case Tree.Node(PRN, Stream(
        Tree.Node(AtPRN, Stream(
          Tree.Node(LRB, Stream(_)),
          PredicateArgumentsExpression(args)
        )),
        Tree.Node(RRB, Stream(_))
      ))  =>

        args.some

      case PredicateArgumentExpression(arg) =>

        Stream(arg).some

      case Tree.Node(S|AtS|NP|AtNP, Stream(PredicateArgumentsExpression(args))) =>

        args.some

      case Tree.Node(S|AtS, Stream(PredicateArgumentsExpression(args), Tree.Node(_, Stream(_)))) =>

        args.some

      case Tree.Node(S|AtS|NP|AtNP, Stream(PredicateArgumentsExpression(args1), PredicateArgumentsExpression(args2))) =>

        args1.some |+| args2.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Literal]] = apply(tree)

  }

  object PredicateExpression extends ConstituentPattern {

    private[this] val preVB = new Preterminal(VB)
    private[this] val preVBD = new Preterminal(VBD)
    private[this] val preVBG = new Preterminal(VBG)
    private[this] val preVBN = new Preterminal(VBN)
    private[this] val preVBP = new Preterminal(VBP)
    private[this] val preVBZ = new Preterminal(VBZ)

    def apply(tree : Tree[String]) : Option[Stream[Relation]] = tree match {

      case preVB(_) | preVBD(_) | preVBG(_) | preVBN(_) | preVBP(_) | preVBZ(_) =>

        Stream(Relation(literal=Literal(tree.id))).some

      case Tree.Node(VP|AtVP, Stream(preVB(_) | preVBD(_) | preVBG(_) | preVBN(_) | preVBP(_) | preVBZ(_),  PredicateExpression(stream))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream1), PredicateExpression(stream2))) =>

        (stream1 |+| stream2).some

      case Tree.Node(VP|AtVP, Stream(_, PredicateExpression(stream))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), Tree.Node(PRT, Stream(particle)))) =>

        predicates.map((_ : Relation) |+| Relation(literal=Literal(particle.id))).some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PredicateArgumentsExpression(arguments))) =>

        (predicates >>= applyArguments(arguments)).some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream), Tree.Node(ADVP, Stream(Tree.Node(RB, Stream(_)))))) =>

        stream.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(predicates), PhraseExpression((arguments, clauses)))) =>

        (predicates >>= applyArguments(arguments)).some |+| clauses.some

      case Tree.Node(ADJP, Stream(_, PredicateExpression(predicates))) =>

        predicates.some

      case Tree.Node(S|AtS, Stream(PredicateExpression(predicates))) =>

        predicates.some

      case Tree.Node(VP|AtVP, Stream(PredicateExpression(stream), Tree.Node(_, Stream(_)))) =>

        stream.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Relation]] = apply(tree)

  }

  object PrepositionalPhraseExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Tuple2[Stream[Literal], Stream[Relation]]] = tree match {

      case Tree.Node(PP|AtPP, Stream(Tree.Node(IN|VBG|TO, Stream(_)), PredicateArgumentsExpression(arguments))) =>

        (arguments, Stream()).some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(IN|VBG|TO, Stream(_)), PhraseExpression((arguments, clauses)))) =>

        (arguments, clauses).some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(AtPP, Stream(_, Tree.Node(IN|VBG|TO, Stream(_)))), PhraseExpression((arguments, clauses)))) =>

        (arguments, clauses).some

      case Tree.Node(PP|AtPP, Stream(Tree.Node(AtPP, Stream(_, Tree.Node(IN|VBG|TO, Stream(_)))), PredicateArgumentsExpression(arguments))) =>

        (arguments, Stream()).some

      case Tree.Node(PP|AtPP, Stream(
        Tree.Node(IN, Stream(_)),
        Tree.Node(S, Stream(
          PredicateExpression(clauses)
        ))
      )) => 

        (Stream(), clauses).some

      case Tree.Node(PP|AtPP, Stream(
        Tree.Node(IN, Stream(_)),
        Tree.Node(S, Stream(
          Tree.Node(VP, Stream(
            Tree.Node(VBG, Stream(arg0)),
            PrepositionalPhraseExpression((arguments, clauses))
          ))
        ))
      )) => 
      
        (Stream((Literal(arg0.id) #:: arguments).reduce(_ |+| _)), clauses).some

      case _ => none

    }

    def unapply(tree : Tree[String]) :  Option[Tuple2[Stream[Literal], Stream[Relation]]] = apply(tree)

  }

  object PhraseExpression extends ConstituentPattern {

    def apply(tree :  Tree[String]) : Option[Tuple2[Stream[Literal], Stream[Relation]]] = tree match {

      case PrepositionalPhraseExpression((arguments, clauses)) =>

        (arguments, clauses).some

      case Tree.Node(ADJP|AtADJP|NP|AtNP|S|AtS, Stream(PredicateArgumentsExpression(args1), PrepositionalPhraseExpression((args2, clauses)))) =>

        def arguments = args2.flatMap(arg2 => args1.map(arg1 => arg2 |+| arg1))

        (arguments, clauses).some

      case Tree.Node(ADJP|AtADJP|NP|AtNP|S|AtS, Stream(PhraseExpression((args1, clauses1)), PrepositionalPhraseExpression((args2, clauses2)))) =>

        def arguments = args2.flatMap(arg2 => args1.map(arg1 => arg2 |+| arg1))

        (arguments, clauses1 |+| clauses2).some

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
          Tree.Node(WHNP|IN, Stream(
            Tree.Node(_, Stream(_)))),
            Tree.Node(S|AtS, Stream(
              PredicateExpression(predicates)
            ))
        ))
      )) =>

        (arguments, (predicates >>= applyArguments(arguments))).some

      case Tree.Node(NP, Stream(
        PredicateArgumentsExpression(arguments),
        Tree.Node(SBAR, Stream(
          Tree.Node(WHNP, Stream(
            PredicateArgumentsExpression(_),
            Tree.Node(WHPP, Stream(
              Tree.Node(IN, Stream(_)),
              Tree.Node(WHNP, Stream(
                Tree.Node(WDT, Stream(_))
              ))
            ))
          )),
          Tree.Node(S, Stream(
            PredicateExpression(predicates)
          ))
        ))
      )) => 

        (arguments, (predicates >>= applyArguments(arguments))).some

      case Tree.Node(NP|AtNP, Stream(PredicateArgumentsExpression(arguments), PredicateExpression(predicates))) =>

        (arguments, (predicates >>= applyArguments(arguments))).some

      case Tree.Node(S|AtS, Stream(PhraseExpression((arguments, clauses)))) =>

        (arguments, clauses).some

      case Tree.Node(S|AtS, Stream(PhraseExpression((args1, clauses1)), PhraseExpression(args2, clauses2))) =>

        (args1 |+| args2, clauses1 |+| clauses2).some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Tuple2[Stream[Literal], Stream[Relation]]] = apply(tree)

  }

  object ClauseExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Stream[Relation]] = tree match {

      case Tree.Node(S|AtS, Stream(
        PredicateArgumentsExpression(arguments),
        Tree.Node(VP, Stream(
          PredicateExpression(predicates),
          ClauseExpression(clauses)
        ))
      )) =>

        (predicates >>= applyArguments(arguments)).some |+| clauses.some

      case Tree.Node(S|AtS|SBAR|AtSBAR, Stream(ClauseExpression(clauses1), ClauseExpression(clauses2))) =>

        clauses1.some |+| clauses2.some

      case Tree.Node(S|AtS|SBAR|AtSBAR, Stream(ClauseExpression(clauses), Tree.Node(_, Stream(_)))) =>

        clauses.some

      case Tree.Node(S|AtS, Stream(PhraseExpression((arguments, clauses)), PredicateExpression(predicates))) =>

        clauses.some |+| (predicates >>= applyArguments(arguments)).some

      case Tree.Node(S|AtS, Stream(PredicateArgumentsExpression(arguments), PredicateExpression(predicates))) =>

        (predicates >>= applyArguments(arguments)).some

      case Tree.Node(SBAR|AtSBAR, Stream(ClauseExpression(clauses), Tree.Node(_, Stream(_)))) =>

        clauses.some

      case Tree.Node(SBAR|AtSBAR, Stream(ClauseExpression(clauses))) =>

        clauses.some

      case Tree.Node(SBAR|AtSBAR, Stream(Tree.Node(IN, Stream(_)), ClauseExpression(clauses))) =>

        clauses.some

      case _ => none

    }

    def unapply(tree : Tree[String]) : Option[Stream[Relation]] = apply(tree)

  }


  object RootExpression extends ConstituentPattern {

    def apply(tree : Tree[String]) : Option[Stream[Relation]] = tree match {
      case Tree.Node(ROOT, Stream(ClauseExpression(relations))) => relations.some
      case _ => none
    }

  }

}


