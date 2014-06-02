package edu.berkeley.crea.beagle

import java.util.Scanner
import java.io.InputStream

object Tokenize {

  def apply(in : InputStream)(tokenFunction : (String) => Unit) : Unit = {

    val scanner = new Scanner(in)
    scanner.useDelimiter("(?<=\\.+)\\s*(?=[A-Z]*)")

    while(scanner.hasNext()) {
      tokenFunction(scanner.next())
    }

    scanner.close()

  }

}


