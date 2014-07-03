package edu.crea

import java.util.Scanner
import java.io.{ InputStream, ByteArrayInputStream }

import scala.collection.immutable.Stream

object Tokenize {

  def apply(str : String) : Stream[String] = {
    Tokenize(new ByteArrayInputStream(str.getBytes))
  }

  def apply(in : InputStream) : Stream[String] = {

    val scanner = new Scanner(in)
    scanner.useDelimiter("""(?<=[!?.])\s+(?=[A-Z]+)""")

    def stream : Stream[String] = if(scanner.hasNext) {
      scanner.next() #:: stream
    } else {
      scanner.close()
      Stream.empty[String]
    }

    stream

  }

}


