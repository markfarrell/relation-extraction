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

  "Slices" should "contain all possible slices of a node's children." in {
 
    val tree : Tree[String] = "(NP (DT The) (NN dog) (CC and) (NN cat.))"
    val a : Tree[String] = "(DT The)"
    val b : Tree[String] = "(NN dog)"
    val c : Tree[String] = "(CC and)"
    val d : Tree[String] = "(NN cat.)"
    val expectation : Set[Set[LinguisticTree]] = Set[Set[LinguisticTree]](
      Set(a), Set(b, c, d),
      Set(a, b), Set(c ,d),
      Set(a, b, c),  Set(d), 
      Set(), Set(a, b, c ,d),
      Set(b, c), Set(c), Set(b)
    )
    tree.slices().toString() should be (expectation.toString())

  }

  "Power tree" should "contain all possible sliced trees of a tree." in {

    val tree : LinguisticTree = "(NP (DT The) (NN dog) (CC and) (NN cat.))"

    val a : Tree[String] = "(DT The)"
    val b : Tree[String] = "(NN dog)"
    val c : Tree[String] = "(CC and)"
    val d : Tree[String] = "(NN cat.)"
    val possibleChildren : Set[Set[LinguisticTree]] = Set[Set[LinguisticTree]](
      Set(a), Set(b, c, d),
      Set(a, b), Set(c ,d),
      Set(a, b, c),  Set(d), 
      Set(), Set(a, b, c ,d),
      Set(b, c), Set(c), Set(b),
      Set()
    )

    val trees : Set[LinguisticTree] = (possibleChildren map { 
      tree.replaceChildren(_)
    }) | (possibleChildren flatten)

    tree.powerTree().toString() should be (trees.toString())

  }

  "Longest slice" should "should find the longest slice of the power tree containing the provided labels." in { 
     
       val tree : LinguisticTree = "(NP (DT The) (NN dog) (CC and) (NN cat.))"
       val expectation : LinguisticTree = "(NP (DT The) (NN dog))"
       
       tree.longestSlice(Set[String]("DT", "NN")) match { 
         case Some(t) => t.toString() should be (expectation.toString())
         case None => assert(false)
       }
  }
       

} 
