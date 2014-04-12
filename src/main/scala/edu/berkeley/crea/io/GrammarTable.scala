package edu.berkeley.crea.io

import scala.collection.immutable.List
import scala.collection.JavaConverters._

import edu.berkeley.nlp.util.{ 
  Numberer, CollectionUtils
} 

import edu.berkeley.nlp.PCFGLA.{ 
  UnaryRule, BinaryRule, Grammar 
}

/** 
  * @class GrammarTable - Provides utilities for printing information
  * about a PCF grammar and for walking parsed trees in Environment. 
 **/ 
class GrammarTable(grammar : Grammar) {

  /** 
    * @type TableRule - Congruent to a part-of-speech tag pointing
    * to either unary rule or binary rule data in the form of String tuples. 
   **/ 
  type TableRule = (String, Either[String, (String,String)])

  private var lst : List[TableRule] = {

    def addBinaryRule(binaryRule : BinaryRule) : TableRule = { 

      val parentState : String = stateString(binaryRule.getParentState) 
      val leftState : String = stateString(binaryRule.getLeftChildState)
      val rightState : String = stateString(binaryRule.getRightChildState) 

      (parentState,  Right((leftState, rightState)))

    } 

    def addUnaryRule(unaryRule : UnaryRule) : TableRule = { 

      val parentState : String = stateString(unaryRule.getParentState) 
      val childState : String = stateString(unaryRule.getChildState) 

      (parentState, Left(childState))

    } 

    def clip(str : String) : String = if(str.endsWith("^g")) { 
      str.substring(0, str.length - 2) 
    } else { 
      str 
    }  

    def stateString(state : Short) : String = { 

      val numberer : Numberer = Numberer.getGlobalNumberer("tags")
      val str : String = numberer.`object`(state).asInstanceOf[String]

      clip(str) 

    } 

    { 
      val n : Int = grammar.numStates

      val binaryRules : Iterable[TableRule] = for { 
        i <- 0 until n 
        binaryRule <- grammar.splitRulesWithP(i) 
      } yield addBinaryRule(binaryRule) 

      val unaryRules : Iterable[TableRule] = for { 
        i <- 0 until n
        unaryRule <- grammar.getClosedSumUnaryRulesByParent(i) 
      } yield addUnaryRule(unaryRule) 

      (binaryRules ++ unaryRules) toList
    } 

  } 

  override def toString() : String = {

    val ruleStrings : List[String] = this.lst map { 
      (tableRule : TableRule) => tableRule._1 + " -> " + {
        tableRule._2 match {
          case Left(x) => x 
          case Right(x) => x._1 + " " + x._2
        } 
      } 
    }

    { 
      CollectionUtils.sort(ruleStrings.asJava).asScala.toList
    } mkString("\n") 

  }

} 

