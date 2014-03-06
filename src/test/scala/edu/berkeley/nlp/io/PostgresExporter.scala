package edu.berkeley.nlp.io

import java.sql.DriverManager
import java.sql.Connection
import java.sql.SQLException
import java.util.Properties

import org.scalatest._
import org.scalatest.exceptions.TestCanceledException

class PostgresExporterSpec extends FlatSpec with Matchers {

   private val conn : Connection = {

    val host : String = "127.0.0.1"
    val port : Int = 5432
    val db : String = "test"
    val username : String = "m4farrel"
    val password : String = "" 

    Class.forName("org.postgresql.Driver")

    val url : String = "jdbc:postgresql://"+host+":"+port+"/"+db
    val props : Properties = new Properties() 
    props.setProperty("user", username)
    props.setProperty("password", password)
    props.setProperty("ssl", "true")
    props.setProperty("sslfactory", "org.postgresql.ssl.NonValidatingFactory")
    
    try { 
      DriverManager.getConnection(url, props)
    } catch { 
      case e : SQLException => null
    } 

  } 

  "A PostgresExporter" should "export an Environment to a Postgres database." in { 
    assume(conn != null, "Could not connect to PostgresSQL database.")

  }

} 
