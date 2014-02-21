package edu.berkeley.nlp.io

import edu.berkeley.nlp.syntax.Environment
import edu.berkeley.nlp.syntax.TreeConversions._

import com.redis.RedisClient

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

object GexfExporter { 
   
  def main( args : Array[String]) : Unit = {

    var redisHost : Option[String] = None
    var redisPort : Option[Int] = None

    if(args.length == 2) { 
      redisHost = Some(args(0))
      redisPort = try { 
        Some(args(1).toInt)
      } catch { 
        case e : Exception => None
      } 
    } 

    for {
      host <- redisHost
      port <- redisPort
    } yield {

      val redisClient : RedisClient = new RedisClient(host, port)

      var terms : List[Environment.Term] = List()

      for(keys <- redisClient.keys(); Some(key) <- keys) {
        for { 
          value <- redisClient.get(key)
          term <- Environment.toClause(value) 
        } terms = term :: terms
      }

      (new StaxGraphWriter).writeToStream(Environment.toGexf(terms), System.out, "UTF-8")
      
    }

  } 
    
} 
