package edu.berkeley.nlp.syntax

import org.scalatest._
import TreeConversions._


class EnvironmentSpec extends FlatSpec with Matchers  {

  "toDependency" should " produce an Option[Dependency]." in { 
    val tree : LinguisticTree = "((IN after))" 
    val expectation : Option[Environment.Dependency] = Some(Environment.Dependency("after", None))
    println(expectation.toString())
    Environment.toDependency(tree) should be (expectation)
  } 

  "toAction" should " produce an Option[Action]." in { 
    val tree : LinguisticTree = "((VP (VBZ walks) (PP (IN because))))"
    val expectation : Option[Environment.Action] = Some(Environment.Action("walks", Some(Environment.Dependency("because", None))))
    Environment.toAction(tree) should be (expectation)

  }

  "toCondition" should " produce an Option[Condition]." in {
     val tree : LinguisticTree = "(VP (MD could) (VP (VB walk)))" 
     val expectation : Option[Environment.Condition] = Some(Environment.Condition("could", Some(Environment.Action("walk", None))))

     Environment.toCondition(tree).toString() should be (expectation.toString())
  }

  "toTopic" should " produce an Option[Topic]." in { 
     val tree : LinguisticTree = "((NP (DT The) (NN dog) (NN walks.)))"
     val expectation : Option[Environment.Topic] = Some(Environment.Topic(List[String]("The", "dog","walks."), None))

     Environment.toTopic(tree).toString() should be (expectation.toString())

  }

  "toClause" should " produce an Option[Term]." in { 
     val tree : LinguisticTree = "((NP (DT The) (NN dog) (NN walks.)))"
     val expectation : Option[Environment.Term] = Some(Environment.Topic(List[String]("The", "dog","walks."), None))

     Environment.toClause(tree).toString() should be (expectation.toString())

  }

} 
