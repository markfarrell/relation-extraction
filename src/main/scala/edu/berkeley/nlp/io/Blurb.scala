package edu.berkeley.syntax

import java.util.Scanner
import java.io.InputStream

object Blurb { 

  def tokens(in : InputStream) : List[String] = { 

    var tokens : List[String] = List[String]()
    val scanner = new Scanner(System.in)
    scanner.useDelimiter("(?<=\\.+)\\s*(?=[A-Z]*)")

    while(scanner.hasNext()) { 
      tokens = scanner.next() :: tokens
    } 

    scanner.close()

    tokens 

  } 

} 


