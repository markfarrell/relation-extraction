package edu.berkeley.nlp.parser

import scala.io.Source

import java.util.Scanner
import java.io.PrintWriter
import java.io.PrintStream
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.io.FileOutputStream
import java.io.FileDescriptor
import java.io.ByteArrayOutputStream

object LazyCacheBlurb {

  def main(args : Array[String]) {

    val stdout = new PrintStream(new FileOutputStream(FileDescriptor.out))

    val values = {
      var tokens : Array[String] = Array[String]()
      val scanner = new Scanner(System.in)
      scanner.useDelimiter("(?<=\\.+)\\s*(?=[A-Z]*)")

      while(scanner.hasNext()) { 
        tokens = tokens :+ scanner.next() 
      } 

      scanner.close()

      tokens
    } foreach {  
      x => {

        System.setOut(stdout)
        println(x)

        val out = new PipedOutputStream()
        val in = new PipedInputStream(out)
        val writer = new PrintWriter(out)

        System.setIn(in)

        writer.write(x)
        writer.flush()
        writer.close()

        LazyCache.main(args)

        in.close()

      }
    }


  }

}
