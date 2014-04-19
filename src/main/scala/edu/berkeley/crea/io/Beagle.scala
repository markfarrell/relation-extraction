package edu.berkeley.crea.io

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

import edu.berkeley.nlp.syntax.Tree

import edu.berkeley.crea.syntax.TreeConversions._
import edu.berkeley.crea.syntax.Environment
import edu.berkeley.crea.syntax.Environment.{ 
  Term, Topic, Dependency, Condition, Action
}

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

import org.postgresql.util.PSQLException


/**
  * @object Beagle - A tool for exporting environments to Postgres databases,
  * clearing databases, reloading environments saved in databases and compiling
  * environments to Gephi datasets.
 **/
object Beagle {

   /** 
     * @class Config - command line options for the tool.
    **/
   case class Config(
    database : String = "test", 
    host : String = "127.0.0.1",
    port : Int = 5432,
    user : String = "",
    password : String = "", 
    file : File = null,
    grammar : String = "./lib/eng_sm6.gr", 
    clear : Boolean = false, 
    export : Boolean = false,
    dumpgrammar : Boolean = false,
    columns : Int = 4
  )

  /** 
    * Set values for command line options. Specify
    * usage of the tool. 
   **/
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

    opt[Unit]('c', "clear") action { 
      (_, c) => c.copy(clear = true)
    }

    opt[Unit]('e', "export") action { 
      (_, c) => c.copy(export = true) 
    }

    opt[Unit]('D', "dumpgrammar") action {
      (_, c) => c.copy(dumpgrammar = true) 
    } 

    opt[Int]('C', "columns") action { 
      (x, c) => c.copy(columns = x)
    }  

    help("help") text("Prints this help message.")

  } 

  /**
    * @method connect - Connect to the database with 
    * parameters specifyed in cfg.
    * @param cfg {Config} 
    * @return {Connection} - A connection to the database.
   **/
  private def connect(cfg : Config) : Connection = {

       Class.forName("org.postgresql.Driver")

        val url : String = "jdbc:postgresql://"+cfg.host+":"+cfg.port+"/"+cfg.database
        val props : Properties = new Properties()

        if(cfg.user.length > 0) { 
          props.setProperty("user", cfg.user)
        }

        if(cfg.password.length > 0) { 
          props.setProperty("password", cfg.password)
        } 

        DriverManager.getConnection(url, props)

  }

  /** 
    * @method createEnv - Construct an environment from 
    * stdin text.
    * @param cfg {Config} 
    * @return {Environment} 
   **/ 
  def createEnv(cfg : Config) : Environment = {

    val env : Environment = new Environment
    val parser : DefaultParser = new DefaultParser(cfg.grammar)

    def parse(str : String) : Tree[String]  = {
      val ret : Tree[String] = parser.parse(str) 
      println(str + " -> " + ret.toString) 
      ret
    }

    env.insertTopics { 
      Blurb.tokens(System.in) map parse flatMap {
        tree => Environment.toTopic(tree)
      }  
    }

    env

  } 

  /** 
    * @method makeTable 
    * @param columns {Int} - Produce a table with this many columns per row.
    * @param lines {String} - A string with items separated by newline
    * characters. 
    * @return - A function to make tables with said number of columns, which
    * then takes newline-delimited strings and produces HTML tables.
    **/ 
  private def makeTable(columns : Int)(lines : String) : String = {
    for { 
      group <- { lines split("\n") grouped(columns) } 
    } yield {
      val column = group map { s => "<td>" + s + "</td>" } 
      column mkString("<tr>", "", "</tr>") 
    } 
  } mkString("<table>", "", "</table>")  

  /** 
    * @method main - Entry point for the tool.
    * @param args - {Array[String]} 
   **/
  def main(args : Array[String]) : Unit = { 

    parser.parse(args, Config()) map { 
      cfg => {

        if(cfg.dumpgrammar) { 
          println { 
            makeTable(cfg.columns) { 
              new GrammarTable( new DefaultParser(cfg.grammar).getGrammar ).toString
            }        
          } 
        } 

        // Clear existing contents of database
        if(cfg.clear) {

          val conn : Connection = connect(cfg) 

          val statement : Statement = conn.createStatement()

          try { 
            statement.executeQuery("DELETE FROM beagle.topics CASCADE")
          } catch { 
            case e : PSQLException => {}  
          } 

          statement.close()

          conn.close()

        }

        if(cfg.export) {

          val conn : Connection = connect(cfg) 

          val env : Environment = createEnv(cfg) 

          try {

            // Export to database
            (new PostgresExporter(conn,env)).export()

          } catch { 
            case e : Exception => e.printStackTrace()
          } 

          conn.close() 

        } 

        // Maybe produce a GEXF file
        if(cfg.file != null) {

          val conn : Connection = connect(cfg) 

          val fs : FileOutputStream = new FileOutputStream(cfg.file) 

          val env : Environment = new PostgresImporter(conn).load()
          //val env = createEnv(cfg) 

          (new StaxGraphWriter).writeToStream(Environment.toGexf(env.selectTopics()), fs, "UTF-8")

          conn.close() 

        }

      
      }

    }

  }
     
} 
