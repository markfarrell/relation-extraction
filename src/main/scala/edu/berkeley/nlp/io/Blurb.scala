package edu.berkeley.nlp.io

import java.util.Scanner
import java.io.InputStream
import java.io.ByteArrayInputStream

/** 
  * @object Blurb - contains methods to tokenize blurbs 
  * of text (i.e. blocks containing multiple sentences).
 **/
object Blurb {

 /**
   * @method tokens 
   * @param str {String} - the string to tokenize.
   * @return {List[String]} - a list of sentences.
  **/
  def tokens(str : String) : List[String] = { 

    tokens(new ByteArrayInputStream(str.getBytes()))
  } 

 /**
   * @method tokens
   * @param in {InputStream} - Tokenize a blurb contained as bytes in an input stream.
   * @return {List{String}} - a list of sentences.
  **/
  def tokens(in : InputStream) : List[String] = { 

    var tokens : List[String] = List[String]()
    val scanner = new Scanner(in)
    scanner.useDelimiter("(?<=\\.+)\\s*(?=[A-Z]*)")

    while(scanner.hasNext()) { 
      tokens = scanner.next() :: tokens
    } 

    scanner.close()

    tokens.reverse 

  } 

} 


