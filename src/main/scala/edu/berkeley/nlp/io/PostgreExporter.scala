import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.PreparedStatement
import java.sql.SQLException
import java.sql.Types 
import java.util.Properties

import scala.collection.mutable.{HashMap, MultiMap, Set }

import edu.berkeley.nlp.syntax.Environment
import edu.berkeley.nlp.syntax.Environment.Term
import edu.berkeley.nlp.syntax.Environment.Topic
import edu.berkeley.nlp.syntax.Environment.Dependency
import edu.berkeley.nlp.syntax.Environment.Condition
import edu.berkeley.nlp.syntax.Environment.Action

object PostgreExporter {

  // TODO: Create class PostgreExporter, making all fields and methods found in 'export' private 
  def export(conn : Connection, env : Environment) : Unit = {

    class DependenciesTable extends HashMap[Dependency, Set[Action]] with MultiMap[Dependency, Action]
    class TopicsTable extends HashMap[Term, Set[Topic]] with MultiMap[Term, Topic]
    class ConditionsTable extends HashMap[Action, Set[Condition]] with MultiMap[Action, Condition]

    // Initialize field members
    val topics : List[Topic] = env.selectTopics()

    val topicsTable : TopicsTable = new TopicsTable 
    val conditionsTable : ConditionsTable = new ConditionsTable
    val dependenciesTable : DependenciesTable = new DependenciesTable

    val yes: Int = Statement.RETURN_GENERATED_KEYS

    // Methods to produce new prepared statements 
    def insertTopic : PreparedStatement = conn.prepareStatement("INSERT INTO topics VALUES(?)")
    def insertAction : PreparedStatement = conn.prepareStatement("INSERT INTO actions VALUES(?,?,?)", yes)
    def insertCondition : PreparedStatement = conn.prepareStatement("INSERT INTO conditions VALUES(?,?)", yes)
    def insertDependency : PreparedStatement = conn.prepareStatement("INSERT INTO dependencies VALUES(?,?,?)")

    def extractDependencies(action : Action) = for (dependency <- action.dependencies) { 
      dependenciesTable.addBinding(dependency, action) 
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

    // TODO: Methodize blocks found inside this try
    try { 

      // Commit all or nothing, since we want to enforce the same relationship 
      // between terms in the environment. 
      conn.setAutoCommit(false) 

      // Insert all new topics into the database 
      // TODO: What happens if a topic already exists?
      {

        val insertedTopic : PreparedStatement = insertTopic

        for(topic <- topics) { 
          insertedTopic.setString(1, topic.value)
          insertedTopic.addBatch() // Add current parameters to batch
          insertedTopic.clearParameters()
        } 

        insertedTopic.executeBatch()
        insertedTopic.close()

      } 

      // Insert new actions that point to a topic
      { 
        val insertedAction : PreparedStatement = insertAction
        for(kv <- topicsTable.iterator) {
          kv._1 match { 
            case action : Action => { 
              for(topic <- kv._2) { 
                insertedAction.setString(1, action.value)
                insertedAction.setString(2, topic.value)
                insertedAction.setNull(3, Types.INTEGER)
                insertedAction.addBatch() 
                insertedAction.clearParameters()
              }
            }
          }
        }
        insertedAction.executeBatch()
        insertedAction.close()
      } 

      // Insert new conditions that point to a topic
      {
        val insertedCondition : PreparedStatement = insertCondition

        for(kv <- topicsTable.iterator) {
          kv._1 match { 
            case condition : Condition => {
              for (topic <- kv._2) { 
                insertedCondition.setString(1, condition.modal) 
                insertedCondition.setString(2, topic.value) 
                insertedCondition.addBatch() 
                insertedCondition.clearParameters()
              } 
            } 
          } 
        }

        insertedCondition.executeBatch()
        insertedCondition.close()
      }

      // Grab all generated condition keys
      // Insert new actions that point to a condition
      {
        val insertedAction : PreparedStatement = insertAction
      
        for(kv <- conditionsTable) { 

          val action : Action = kv._1
          
          //TODO : ...
        } 

        insertedAction.executeBatch()
        insertedAction.close()
      } 

      // Grab all generated action keys
      // Insert dependencies that point to actions
      // TODO: ...

      conn.commit() // Complete transaction

    } catch { 
      case e : SQLException => { 
        conn.rollback()
        println(e.getStackTrace)
      } 
    } finally { 
      conn.setAutoCommit(true)

      // Allow database to release resources 
      insertAction.close()
      insertCondition.close()
      insertDependency.close()

    } 

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
