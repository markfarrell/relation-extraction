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
import java.io.FileOutputStream
import java.io.FileDescriptor
import java.io.IOException
import java.net.UnknownHostException
import java.security.Permission

import com.redis.RedisClient
import edu.berkeley.nlp.PCFGLA.BerkeleyParser
import edu.berkeley.nlp.util.CommandLineUtils

/** 
  * Memoizes the results of parsing sentences with the BerkeleyParser.
  * under a certain configuration.
  *
  * @constructor 
  * @param host A Redis host -- for where results are cached.
  * @param port A Redis port -- for where results are cached.
  * @param args Flags used to configure the BerkeleyParser
 **/
class LazyCache( host : String , port : Int, args : Array[String] ) {

  private[this] val client = new RedisClient(host, port)

  def clear() = client.flushdb
  
  def get(sentence : String) : String = { 

    System.setIn({ 

      val out : PipedOutputStream = new PipedOutputStream()
      val in : PipedInputStream = new PipedInputStream(out)
      val writer : PrintWriter = new PrintWriter(out)

      System.setIn(in)

      writer.write(sentence)
      writer.flush()
      writer.close()

      in
    })

    client.get(sentence) match { 
      case None => {

        val stdout = new PrintStream(new FileOutputStream(FileDescriptor.out))

        val captureStream : PrintStream = new PrintStream(System.out) {

          val captured : StringBuilder = new StringBuilder()

          @throws[IOException]()
          override def write(buf : Array[Byte], off : Int, len : Int) {

            //super.write(buf, off, len)
            captured.appendAll(buf map { b => b.asInstanceOf[Char] }, off, len)

          }

          @throws[IOException]() 
          override def write(b : Int) {

            //super.write(b)
            captured.append(b.asInstanceOf[Char])

          }

          override def toString() : String = {
            captured.toString()
          }

          override def close() { 
            client.set(sentence, captured.toString())
          }

        }

        System.setOut(captureStream)

        // Temporary prevent the BerkeleyParser tool 
        // from causing the process to exit
        System.setSecurityManager( new SecurityManager() {

            override def checkExit( status : Int ) {

              throw new SecurityException() 
            }

            override def checkPermission( permission : Permission ) { }
        })


        try { 
          BerkeleyParser.main(args)
        } catch { 
          case ex : SecurityException => {} 
        }

        System.setSecurityManager( null )

        System.setOut(stdout)

        captureStream.close()
        captureStream.toString()

      }
      case Some(s) => s
    }

  }

}

/** 
 * Companion object for LazyCache, 
 * having an entry point for when the tool is used on
 * the command line.
 **/
object LazyCache {

  /**
    * Credit: https://github.com/kstyrc/embedded-redis
   **/
  def spawnRedisServer(port : Integer) : Process = {

    val redisServer = new ProcessBuilder("redis-server", "--port", Integer.toString(port)).start()

  
    val readyPattern : String = ".*The server is now ready to accept connections on port.*" 

    val reader : BufferedReader = new BufferedReader(new InputStreamReader(redisServer.getInputStream()));

    try {
      var outputLine : String = null;
      do {
        outputLine = reader.readLine();
      } while (outputLine != null && !outputLine.matches(readyPattern));
    } finally {
      reader.close();
    }
    

    redisServer

  }

  def main(unfilteredArgs : Array[String]) {

    var shouldClear = false
    var host : String = null
    var port : Integer = 6379

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

      argMap = chop(argMap, "-clear", (value : String) => { shouldClear = true })

      argMap.flatMap(p => Array(p._1, p._2)).toArray[String]

    }

    val start = () => { 

      val lazyCache : LazyCache = new LazyCache(host, port, args)

      if(shouldClear) lazyCache.clear()
      else println(lazyCache.get((new BufferedReader(new InputStreamReader(System.in))).readLine()))

    }

    if(host == null) {

      val redisServer : Process = spawnRedisServer(port)

      start()

      redisServer.destroy()

    } else { 

      start()

    } 

  }
} 
