package edu.berkeley.nlp.io

import org.scalatest._ 

class BlurbSpec extends FlatSpec with Matchers {

  "token" should "produce a list of sentences in a blurb." in { 

    val test : String = "A man walks to the store. A boy walks to the store." 

    val expected : List[String] = List[String](
      "A man walks to the store.", 
      "A boy walks to the store."
    ) 

    Blurb.tokens(test) should be (expected)
  } 

} 


