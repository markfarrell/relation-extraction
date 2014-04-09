package edu.berkeley.nlp.io

import scala.collection.JavaConverters._

import edu.berkeley.nlp.util.Numberer
import edu.berkeley.nlp.util.CollectionUtils 
import edu.berkeley.nlp.PCFGLA.UnaryRule
import edu.berkeley.nlp.PCFGLA.BinaryRule
import edu.berkeley.nlp.PCFGLA.Grammar 

/** 
  * @object Grammars - Printing utilities for grammars 
  * and their rules. 
 **/ 
object GrammarStrings {

  /**
    * @method convertGrammar - Produce a string of information 
    * about a grammar.
    * @param grammar {Grammar} 
    * @return {String} 
   **/ 
  def convertGrammar(grammar : Grammar) : String = { 

    val n : Int = grammar.numStates

    val binaryStrings : Iterable[String] = for { 
      i <- 0 until n 
      binaryRule <- grammar.splitRulesWithP(i) 
    } yield convertBinaryRule(binaryRule) 

    val unaryStrings : Iterable[String] = for { 
      i <- 0 until n
      unaryRule <- grammar.getClosedSumUnaryRulesByParent(i) 
    } yield convertUnaryRule(unaryRule) 

    val ruleStrings : List[String] = { 
      
      val lst : List[String] = { 
        binaryStrings ++ unaryStrings
      } toList

      CollectionUtils.sort(lst.asJava).asScala.toList

    } 

    ruleStrings.mkString("\n") 

  } 

  /** 
    * @method convertBinaryRule - Produce information about a binary rule.
    * @param binaryRule {BinaryRule} 
    * @return {String} 
   **/ 
  def convertBinaryRule(binaryRule : BinaryRule) : String = { 
    
    val parentState : String = stateString(binaryRule.getParentState) 
    val leftState : String = stateString(binaryRule.getLeftChildState)
    val rightState : String = stateString(binaryRule.getRightChildState) 

    parentState + " -> " + leftState + " " + rightState

  } 

  /** 
    * @method convertUnaryRule - Produce information about a unary rule.
    * @param unaryRule {UnaryRule} 
    * @return {String} 
  **/
  def convertUnaryRule(unaryRule : UnaryRule) : String = { 
    
    val parentState : String = stateString(unaryRule.getParentState) 
    val childState : String = stateString(unaryRule.getChildState) 

    parentState + " -> " + childState

  } 

  /** 
    * @method clip 
    * @param str {String} 
    * @return {String} 
   **/ 
  private def clip(str : String) = if(str.endsWith("^g")) { 
    str.substring(0, str.length - 2) 
  } else { 
    str 
  }  

  /** 
    * @method stateString
    * @param state {Short} 
    * @return {String} 
   **/
  private def stateString(state : Short) = { 

    val numberer : Numberer = Numberer.getGlobalNumberer("tags")
    val str : String = numberer.`object`(state).asInstanceOf[String]

    clip(str) 

  } 

} 
