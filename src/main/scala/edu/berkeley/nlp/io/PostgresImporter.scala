import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.PreparedStatement
import java.sql.SQLException
import java.sql.Types
import java.sql.ResultSet
import java.util.Properties

import scala.collection.mutable.HashMap

import edu.berkeley.nlp.syntax.Environment
import edu.berkeley.nlp.syntax.Environment.Term
import edu.berkeley.nlp.syntax.Environment.Topic
import edu.berkeley.nlp.syntax.Environment.Dependency
import edu.berkeley.nlp.syntax.Environment.Condition
import edu.berkeley.nlp.syntax.Environment.Action

/**
  * @class PostgresImporter
  * 
  * @constructor Import an entire environment from a PostgreSQL database. 
  * Useful for testing the success of exporting an environment in to a test
  * database.
 **/
class PostgreImporter(conn : Connection) { 

  private class ConditionMetadata extends HashMap[Int, String]
  private class ActionMetadata extends HashMap[Int, String]
  private class DependencyMetadata extends HashMap[Int, String]
  private case class TopicMetadata(value : String, conditionMetadata : ConditionMetadata, actionMetadata : ActionMetadata) 

  private class DependencyGraph extends HashMap[ActionMetadata, (DependencyMetadata, TopicMetadata)] 

  private def selectTopics() : PreparedStatement = conn.prepareStatement("SELECT * FROM topics")
  private def selectAbsoluteActions() : PreparedStatement  = conn.prepareStatement("SELECT * FROM actions WHERE topic_id = ?")
  private def selectConditionalActions() : PreparedStatement = conn.prepareStatement("SELECT * FROM actions WHERE condition_id = ?")
  private def selectConditions() : PreparedStatement = conn.prepareStatement("SELECT * FROM conditions WHERE topic_id = ?") 
  private def selectDependencies() : PreparedStatement = conn.prepareStatement("SELECT * FROM dependencies WHERE action_id = ?") 

  /**
    * @method getTopicList
    * @return A list of unique topic IDs.
   **/
  private def getTopicList() : List[String] = {

    var topicValues : List[String] = List.empty[String]

    val selectedTopics : PreparedStatement = selectTopics()

    val rs : ResultSet = selectTopics.executeQuery()

    while( rs.next() ) { 

      topicValues = rs.getString(1) :: topicValues

    } 

    topicValues 
  } 

  /**
    * @method getAbsoluteActionMetadata
    * @param topicId 
    * @return 
   **/
  private def getAbsoluteActionMetadata(topicId : String) : ActionMetadata = { 
    val selectedAbsoluteActions : PreparedStatement = selectAbsoluteActions()
    selectedAbsoluteActions.setString(1, topicId)
    
    {
      var map : ActionMetadata = new ActionMetadata
      val rs : ResultSet = selectedAbsoluteActions.executeQuery()
      while(rs.next()) { 
        map += (rs.getInt(1) -> rs.getString(2)) 
      } 
      selectedAbsoluteActions.close()
      map
    } 
    
  }

  private def getConditionalMetadata(conditionId : Int) : ActionMetadata = { 
    val selectedConditionalActions : PreparedStatement = selectConditionalActions()
    selectedConditionalActions.setInt(1, conditionId)

    { 
      var map : ActionMetadata = new ActionMetadata
      val rs : ResultSet = selectedConditionalActions.executeQuery()
      while(rs.next()) { 
        map += (rs.getInt(1) -> rs.getString(2))
      } 
      selectedConditionalActions.close()
      map
    } 

  } 

  def load() : Environment = {

    val env : Environment = new Environment

    // TODO:
    // Finish metadata methods 
    //
    // Construct all TopicMetadata
    // Create empty DependencyGraph
    // For each TopicMetadata, take its ActionMetadata and select dependencies for each action id
    // Insert each (DependencyMetadata, TopicMetadata) result into the table IF it does not already exist (to avoid cycles) 

    env 
  } 

} 
