package edu.berkeley.nlp.syntax

import org.scalatest._

import TreeConversions._ 

class TreeConversionsSpec extends FlatSpec with Matchers  {

  "An s-expression" should "be converted into a valid Tree." in {
    val expression : String = "(ROOT (NP (NN Test)))"
    val tree : Tree[String] = "(ROOT (NP (NN Test)))"
    tree.toString() should be (expression)
  }

  "A linguistic tree" should "be converted into a list of terminal labels." in { 
    val tree : Tree[String] = "((S (NP (VBG Walking)) (VP (VBZ is) (ADJP (JJ tiring.)))))"
    tree.terminalList() should be (List[String]("Walking", "is", "tiring."))
  }

} 
