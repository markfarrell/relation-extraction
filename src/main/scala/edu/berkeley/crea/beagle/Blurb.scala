package edu.berkeley.crea.beagle

/**
  * Sentencizes blocks of text.
 **/
object Blurb {

  import java.io.ByteArrayInputStream
  import java.io.InputStream

 /**
   * @method tokens
   * @param str - the string to tokenize.
   * @return A list of sentences.
  **/
  def tokens(str : String) : List[String] = {

    tokens(new ByteArrayInputStream(str.getBytes()))
  }

 /**
   * @method tokens
   * @param in - The inputstream whose contents should be tokenized.
   * @return A list of sentences.
  **/
  def tokens(in : InputStream) : List[String] = {

    var tokenList : List[String] = List.empty[String]

    Tokenize(in) {
      token => tokenList ::= token
    }

    tokenList.reverse

  }

}

