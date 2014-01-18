package edu.berkeley.nlp.parser

import scala.sys
import scala.collection.mutable.StringBuilder
import scala.collection.mutable.Map
import scala.collection.JavaConverters._

import java.io.PrintStream
import java.io.PrintWriter
import java.io.InputStreamReader
import java.io.BufferedReader
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.io.IOException
import java.net.UnknownHostException

import com.redis.RedisClient
import edu.berkeley.nlp.PCFGLA.BerkeleyParser
import edu.berkeley.nlp.util.CommandLineUtils

/** 
 * Companion object for the MemoizedParser, 
 * having an entry point for when the tool is used on
 * the command line.
 **/
object MemoizedParser {

  def main(unfilteredArgs : Array[String]) {

    var shouldFlush = false
    var host = "localhost"
    var port = 6379

    val args = {

      val chop = (argMap: Map[String,String], key : String, action : (String) => Unit ) => {
        argMap.get(key) match { 
          case None => argMap
          case Some(s) => {
            action(s)
            argMap.filter(_._1 != key)  
          }
        }
      }

      var argMap = CommandLineUtils.simpleCommandLineParser(unfilteredArgs).asScala

      argMap = chop(argMap, "-host", (value : String) => { host = value })
      argMap = chop(argMap, "-port", (value : String) => { 
        try { 
          port = Integer.parseInt(value) 
        } catch {
          case ex : NumberFormatException => {} 
        } 
      })

      argMap = chop(argMap, "-clear", (value : String) => { shouldFlush = true })

      argMap.flatMap(p => Array(p._1, p._2)).toArray[String]
 
    }

    val client : RedisClient = { 
      try {
        new RedisClient(host, port)
      } catch {
        case ex : RuntimeException => {

          try {
            Runtime.getRuntime().exec("redis-server &")
            new RedisClient()
          } catch { 
            case ex : Exception => { 
              System.err.println("Please have a redis server running on host " + host + ":" + port)
              System.exit(1) 
              null
            }
          }

        }
      }
    } 

    if(shouldFlush) client.flushdb

    val pipe = () => { 

      val input = (new BufferedReader(new InputStreamReader(System.in))).readLine() 

      val out : PipedOutputStream = new PipedOutputStream()
      val in : PipedInputStream = new PipedInputStream(out)
      val writer : PrintWriter = new PrintWriter(out)

      System.setIn(in)

      writer.write(input)
      writer.flush()
      writer.close()

      input
    }
 
    val input = pipe()

    client.get(input) match { 
      case None => {

        System.setOut(new PrintStream(System.out) {

          var captured = new StringBuilder()

          @throws[IOException]()
          override def write(buf : Array[Byte], off : Int, len : Int) {

            super.write(buf, off, len)
            captured.appendAll(buf map { b => b.asInstanceOf[Char] }, off, len)

          }

          @throws[IOException]()
          override def write(b : Int) {

              super.write(b)
              captured.append(b.asInstanceOf[Char])

          }

          override def close() { 

              client.set(input, captured.toString())

          }

        })

         BerkeleyParser.main(args)

      }
      case Some(s) => println(s)
    }

  }
} 
