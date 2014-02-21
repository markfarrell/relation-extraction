import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.PreparedStatement
import java.util.Properties

import scala.collection.mutable.{HashMap, MultiMap, Set }

import edu.berkeley.nlp.syntax.Environment
import edu.berkeley.nlp.syntax.Environment.Term
import edu.berkeley.nlp.syntax.Environment.Topic
import edu.berkeley.nlp.syntax.Environment.Dependency
import edu.berkeley.nlp.syntax.Environment.Condition
import edu.berkeley.nlp.syntax.Environment.Action

object PostgreExporter {

  def export(conn : Connection, env : Environment) : Unit = {

    class DependenciesTable extends HashMap[Topic, Set[Dependency]] with MultiMap[Topic, Dependency]
    class TopicsTable extends HashMap[Term, Set[Topic]] with MultiMap[Term, Topic]
    class ConditionsTable extends HashMap[Action, Set[Condition]] with MultiMap[Action, Condition]

    val topics : List[Topic] = env.selectTopics()

    val yes: Int = Statement.RETURN_GENERATED_KEYS

    val insertTopic : PreparedStatement = conn.prepareStatement("INSERT INTO topics VALUES(?)")
    val insertAction : PreparedStatement = conn.prepareStatement("INSERT INTO actions VALUES(?,?,?)", yes)
    val insertCondition : PreparedStatement = conn.prepareStatement("INSERT INTO conditions VALUES(?,?)", yes)
    val insertDependency : PreparedStatement = conn.prepareStatement("INSERT INTO dependencies VALUES(?,?,?)", yes)

    val topicsTable : TopicsTable = new TopicsTable 
    val conditionsTable : ConditionsTable = new ConditionsTable

    val dependenciesTable : DependenciesTable = {

      val table : DependenciesTable = new DependenciesTable

      for(topic <- topics) { 
        table += topic -> Set.empty[Dependency]
      } 

      table
    }

    def extractDependencies(action : Action) : Unit = for(dependency <- action.dependencies) { 

      for(clause <- dependency.clauses) { 
        dependenciesTable.addBinding(clause, dependency)
      } 

    }

    def extract() : Unit = for (topic <- topics) {

      for(term <- topic.abilities) { 

        topicsTable.addBinding(term, topic)

        term match {
          case condition : Condition => for (action <- condition.actions ) { 
            conditionsTable.addBinding(action, condition) 
            extractDependencies(action)
          }
          case action : Action => extractDependencies(action) 
        }

      }

    }
    
    extract()
    
    // Iterate through lists, setting preparedStatement values and using executeUpdate()

    for(topic <- topics) { 
      insertTopic.setString(1, topic.value)
      insertTopic.executeUpdate() // TODO: Change to addBatch() 
      insertTopic.clearParameters()
    } 

    for(kv <- topicsTable.iterator) {
      kv._1 match { 
        case condition : Condition => { 
        } 
        case action : Action => { 
        } 
      } 
    }

    /** Execute batch. Commit transaction or rollback. **/

    // Allow database to release resources 
    insertTopic.close()
    insertAction.close()
    insertCondition.close()
    insertDependency.close()

  } 

  def main(args : Array[String]) : Unit = { 
    
     var postgreHost : Option[String] = None
     var postgrePort : Option[Int] = None
     var postgreUsername : Option[String] = None
     var postgrePassword : Option[String] = None

     if(args.length == 4) { 
       postgreHost = Some(args(0))
       postgrePort = try { 
         Some(args(1).toInt)
       } catch { 
         case e : Exception => None
       } 
       postgreUsername = Some(args(2))
       postgrePassword = Some(args(3))
     } 

     for { 
       host <- postgreHost
       port <- postgrePort
       username <- postgreUsername
       password <- postgrePassword
     } yield { 
       
       Class.forName("org.postgresql.Driver")

       val url : String = "jdbc:postgresql://"+host+":"+port+"/beagle" 
       val props : Properties = new Properties() 
       props.setProperty("user", username)
       props.setProperty("password", password)
       props.setProperty("ssl", "true")
       props.setProperty("sslfactory", "org.postgresql.ssl.NonValidatingFactory")
       
       val conn : Connection = DriverManager.getConnection(url, props)
        
     } 

     
  } 
} 
