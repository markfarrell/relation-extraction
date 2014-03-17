package edu.berkeley.nlp.io

import java.io.File
import java.io.FileOutputStream

import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.PreparedStatement
import java.sql.SQLException
import java.sql.Types
import java.sql.ResultSet
import java.util.Properties

import scala.collection.mutable.{HashMap, MultiMap, Set}

import edu.berkeley.nlp.PCFGLA.StreamParser

import edu.berkeley.nlp.syntax.Tree

import edu.berkeley.nlp.syntax.TreeConversions._
import edu.berkeley.nlp.syntax.Environment
import edu.berkeley.nlp.syntax.Environment.Term
import edu.berkeley.nlp.syntax.Environment.Topic
import edu.berkeley.nlp.syntax.Environment.Dependency
import edu.berkeley.nlp.syntax.Environment.Condition
import edu.berkeley.nlp.syntax.Environment.Action

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

object Beagle {

  case class Config(database : String = "test", host : String = "127.0.0.1",
    port : Int = 5432, user : String = "", password : String = "", file : File = null,
    grammar : String = "./lib/eng_sm6.gr", clear : Boolean = false
  )

  val parser = new scopt.OptionParser[Config]("Beagle") { 

    head(
      """Beagle ---
      Reads a blurb of text from STDIN. Parses sentences and loads them 
      into an environment, to be either exported to a PostgresSQL database or 
      compiled to a GEXF file for viewing the Gephi graph visualization software."""
    )

    opt[String]('d', "database") action { 
      (x, c) => c.copy(database = x)
    } text("database is a string property")

    opt[String]('h', "host") action { 
      (x, c) => c.copy(host = x) 
    } text("host is a string property")

    opt[Int]('p', "port") action { 
      (x, c) => c.copy(port = x)
    } text("port is an integer property")

    opt[String]('U', "user") action { 
      (x, c) => c.copy(user = x) 
    } text("user is a string property")

    opt[String]('P', "password") action { 
      (x, c) => c.copy(password = x)
    } text("password is a string property")

    opt[File]('f', "file") action { 
      (x, c) => c.copy(file = x)  
    } text("file is a (GEXF) file property")

    opt[String]('g', "grammar") action {
      (x, c) => c.copy(grammar = x) 
    } text("grammar is a string (path) property")

    opt[Boolean]('c', "clear") action { 
      (x, c) => c.copy(clear = true)
    }

    help("help") text("Prints this help message.")


  } 

  def main(args : Array[String]) : Unit = { 

    parser.parse(args, Config()) map { 
      cfg => {  

        Class.forName("org.postgresql.Driver")

        val url : String = "jdbc:postgresql://"+cfg.host+":"+cfg.port+"/"+cfg.database
        val props : Properties = new Properties()

        if(cfg.user.length > 0) { 
          props.setProperty("user", cfg.user)
        }

        if(cfg.password.length > 0) { 
          props.setProperty("password", cfg.password)
        } 

        val conn : Connection = DriverManager.getConnection(url, props)

        val env : Environment = new Environment

        env.insertTopics { 
          Blurb.tokens(System.in) map { 
            str => { 
              val ret = StreamParser.parseString(str, Array[String]("-gr", cfg.grammar))
              println(ret)
              ret
            } 
          } flatMap {
            str => Environment.toTopic(str)
          }  
        }

        // Clear existing contents of database
        if(cfg.clear) {

          val statement : Statement = conn.createStatement() 
          statement.executeQuery("DELETE FROM beagle.topics CASCADE")
          statement.close() 
        } 

        try { 
          // Export to database
          // (new PostgresExporter(conn,env)).export()
        } catch { 
          case e : Exception => e.printStackTrace()
        } 

        // Maybe produce a GEXF file
        if(cfg.file != null) {
          val fs : FileOutputStream = new FileOutputStream(cfg.file) 
          (new StaxGraphWriter).writeToStream(Environment.toGexf(env.selectTopics()), fs, "UTF-8")
        }

        conn.close()
      }

    }

  }
     
} 
