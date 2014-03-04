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

  private class Metadata extends HashMap[Int, String]
  private class ConditionMetadata extends Metadata
  private class ActionMetadata extends Metadata
  private case class DependencyMetadata(id : Int, value : String, actionId : Int)
  private case class TopicMetadata(value : String, conditionMetadata : ConditionMetadata, actionMetadata : ActionMetadata) 

  private class DependencyGraph extends HashMap[DependencyMetadata, String] 

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

  private def fetchMetadata[T <: Metadata](ps : PreparedStatement, map : T) : T = { 
    val rs : ResultSet = ps.executeQuery()
    while(rs.next()) { 
      map += (rs.getInt(1) -> rs.getString(2))
    } 
    ps.close()
    map
  } 

  /**
    * @method getAbsoluteActionMetadata
    * @param topicId 
    * @return 
   **/
  private def getAbsoluteActionMetadata(topicId : String) : ActionMetadata = {

    val selectedAbsoluteActions : PreparedStatement = selectAbsoluteActions()
    selectedAbsoluteActions.setString(1, topicId)
    
    fetchMetadata[ActionMetadata](selectedAbsoluteActions, new ActionMetadata) 
    
  }

  private def getConditionalActionMetadata(conditionId : Int) : ActionMetadata = { 

    val selectedConditionalActions : PreparedStatement = selectConditionalActions()
    selectedConditionalActions.setInt(1, conditionId)

    fetchMetadata[ActionMetadata](selectedConditionalActions, new ActionMetadata) 

  } 

  private def getActionMetadata(topicMetadata : TopicMetadata) : ActionMetadata = {

    var actionMetadata : ActionMetadata = topicMetadata.actionMetadata

    // Extract all actions that point to the conditions that points to the topic
    val ls : List[(Int, String)] = {
      topicMetadata.conditionMetadata.keys flatMap { 
        getConditionalActionMetadata(_)
      } toList
    } 

    // Iterate and add each entry (keeps the type of ActionMetadata)
    for(l <- ls) { 
      actionMetadata += l
    } 

    actionMetadata 
  } 

  private def getConditionMetadata(topicId : String) : ConditionMetadata = { 

    val selectedConditions : PreparedStatement = selectConditions()
    selectedConditions.setString(1, topicId) 

    fetchMetadata[ConditionMetadata](selectedConditions, new ConditionMetadata) 

  }

  private def getDependencyGraph(actionId : Int, topicId : String) : DependencyGraph = { 
    
    val selectedDependencies : PreparedStatement = selectDependencies() 
    selectedDependencies.setInt(1, actionId)

    {   

      val graph : DependencyGraph = new DependencyGraph
      val ps : PreparedStatement = selectDependencies()
      val rs : ResultSet = ps.executeQuery()

      while(rs.next()) {

        val dependencyMetadata : DependencyMetadata = DependencyMetadata(rs.getInt(1), rs.getString(2), rs.getInt(3)) 
        val topicId : String = rs.getString(4)

        graph += dependencyMetadata -> topicId

      } 

      ps.close()
      graph

    }

  }

  private def getTopicMetadata(topicId : String) : TopicMetadata = { 

    val actionMetadata : ActionMetadata = getAbsoluteActionMetadata(topicId)
    val conditionMetadata : ConditionMetadata = getConditionMetadata(topicId) 
    
    TopicMetadata(topicId, conditionMetadata, actionMetadata) 
    
  }

 
  def load() : Environment = {

    val env : Environment = new Environment

    val topicList : List[String] = getTopicList

    val topicsMetadata : List[TopicMetadata] = topicList map { 
      getTopicMetadata(_)
    }

    var topics : List[Topic] = List.empty[Topic]

    { 

      for(topicMetadata <- topicsMetadata) {

        val topic : Topic = Topic(topicMetadata.value, List.empty[Term])

        val actionMetadata : ActionMetadata = getActionMetadata(topicMetadata) 
        // Get rid of getActionMetadata; 
        // Create Action, or Condition that points to Actions 

        for { 
          actionId <- actionMetadata.keys
          tuple <- getDependencyGraph(actionId, topicMetadata.value)
        } yield Dependency(tuple._1.value, List(Topic(tuple._2, List.empty[Term])))
        // How will dependencies merge together? 

        // Create dependency that points to its Topic (with no terms) 
        // Correct topic will be reconstructed when inserted into the environment

        topics = topic :: topics

      }

    } 

    env.insertTopics(topics)

 
    env 
  } 

} 
