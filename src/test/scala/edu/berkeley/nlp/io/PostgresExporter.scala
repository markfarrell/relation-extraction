package edu.berkeley.nlp.io

import edu.berkeley.nlp.syntax.Environment
import edu.berkeley.nlp.syntax.Environment.Term
import edu.berkeley.nlp.syntax.Environment.Topic
import edu.berkeley.nlp.syntax.Environment.Action
import edu.berkeley.nlp.syntax.Environment.Condition
import edu.berkeley.nlp.syntax.Environment.Dependency

import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.SQLException
import java.util.Properties

import org.postgresql.util.PSQLException

import org.scalatest._
import org.scalatest.exceptions.TestCanceledException

class PostgresExporterSpec extends FlatSpec with Matchers {

   private val conn : Connection = {

    val host : String = "127.0.0.1"
    val port : Int = 5432
    val db : String = "test"

    Class.forName("org.postgresql.Driver")

    val url : String = "jdbc:postgresql://"+host+":"+port+"/"+db
    
    try { 
      DriverManager.getConnection(url)
    } catch { 
      case e : SQLException => null
    } 

  } 

  "A PostgresExporter" should "export an Environment to a Postgres database." in { 

    assume(conn != null, "Could not connect to PostgresSQL database.")

    def clear() = try {
      val statement : Statement = conn.createStatement() 
      statement.executeQuery("DELETE FROM beagle.topics CASCADE") // statement will close automatically
      statement.close()
    } catch { 
      case p : PSQLException => { /** Ignore. No results returned. **/ } 
    } 


    def reloaded(env : Environment) : Environment = {
      
      val exporter : PostgresExporter = new PostgresExporter(conn,env)
      val importer : PostgresImporter = new PostgresImporter(conn)

      var reloadedEnv : Environment = null

      try { 
        exporter.export()

        reloadedEnv = importer.load()
      } catch { 
        case e : Exception => e.printStackTrace()
      } // Allow any contents inserted to still be deleted if there is an exception 

      clear()  

      reloadedEnv

    } 

    clear() // DB should be empty for tests to run.

    // Case 1: Empty environment 
    { 
      val env : Environment = new Environment

      assert(env.size == reloaded(env).size)
    }

    // Case 2: Only a topic  
    { 
      val env : Environment = new Environment

      env.insertTopics(List[Topic](Topic("The dog", List.empty[Term])))

      assert(env.size == reloaded(env).size)

    } 

    // Case 3: Topics with conditons, actions, and dependencies 
    {
      val env : Environment = new Environment

      env.insertTopics(List[Topic](Topic("The man", 
        List(Condition("might", 
          List(Action("hunt", 
            List(Dependency("if", 
              List(Topic("the dog", 
                List(Condition("can", 
                  List(Action("run.", List.empty[Dependency])))))))))))))))

      assert(env.size == reloaded(env).size) 
    } 

    
  }

} 
