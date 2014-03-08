package edu.berkeley.nlp.io

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
class PostgresImporter(conn : Connection) { 

  /** 
    * Importer types 
   **/
  private class Metadata extends HashMap[Int, String]
  private class ConditionMetadata extends Metadata
  private class ActionMetadata extends Metadata
  private case class DependencyMetadata(id : Int, value : String, actionId : Int)
  private case class TopicMetadata(value : String, conditionMetadata : ConditionMetadata, actionMetadata : ActionMetadata) 

  private class DependencyGraph extends HashMap[DependencyMetadata, String] 

  /** 
    * @method selectTopics
    * @return {PreparedStatement}
   **/
  private def selectTopics() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.topics")

  /**
    * @method selectedAbsoluteActions
    * @return {PreparedStatement}
   **/
  private def selectAbsoluteActions() : PreparedStatement  = conn.prepareStatement("SELECT * FROM beagle.actions WHERE topic_id = ?")

  /**
    * @method selectConditionalActions
    * @return {PreparedStatement}
   **/
  private def selectConditionalActions() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.actions WHERE condition_id = ?")

  /**
    * @method selectConditions
    * @return {PreparedStatement} 
   **/
  private def selectConditions() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.conditions WHERE topic_id = ?") 

  /**
    * @method selectDependencies
    * @return {PreparedStatement} 
   **/
  private def selectDependencies() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.dependencies WHERE action_id = ?") 

  /**
    * @method getTopicList
    * @return A list of unique topic IDs.
   **/
  private def getTopicList() : List[String] = {

    var topicValues : List[String] = List.empty[String]

    val selectedTopics : PreparedStatement = selectTopics()

    val rs : ResultSet = selectTopics.executeQuery()

    while(rs.next()) { 

      topicValues = rs.getString(1) :: topicValues

    } 

    topicValues 
  }

  /**
    * @method fetchMetadata {T extends Metadata}
    * @param ps {PreparedStatement}
    * @param map {T}
    * @return {T}
   **/
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
    * @return {ActionMetadata} 
   **/
  private def getAbsoluteActionMetadata(topicId : String) : ActionMetadata = {

    val selectedAbsoluteActions : PreparedStatement = selectAbsoluteActions()
    selectedAbsoluteActions.setString(1, topicId)
    
    fetchMetadata[ActionMetadata](selectedAbsoluteActions, new ActionMetadata) 
    
  }

  /**
    * @method getConditionalActionMetadata
    * @param conditionId {Int}
    * @return {ActionMetadata}
   **/
  private def getConditionalActionMetadata(conditionId : Int) : ActionMetadata = { 

    val selectedConditionalActions : PreparedStatement = selectConditionalActions()
    selectedConditionalActions.setInt(1, conditionId)

    fetchMetadata[ActionMetadata](selectedConditionalActions, new ActionMetadata) 

  } 

  /** 
    * @method getConditionMetadata
    * @param topicId {String}
    * @return {ConditionMetadata}
   **/
  private def getConditionMetadata(topicId : String) : ConditionMetadata = { 

    val selectedConditions : PreparedStatement = selectConditions()
    selectedConditions.setString(1, topicId) 

    fetchMetadata[ConditionMetadata](selectedConditions, new ConditionMetadata) 

  }

  /**
    * @method getDependencyGraph
    * @param actionId {Int} 
    * @return DependencyGraph
   **/
  private def getDependencyGraph(actionId : Int) : DependencyGraph = { 

    val graph : DependencyGraph = new DependencyGraph

    val ps : PreparedStatement = selectDependencies()
    ps.setInt(1, actionId)

    val rs : ResultSet = ps.executeQuery()

    while(rs.next()) {

      val dependencyMetadata : DependencyMetadata = DependencyMetadata(
        rs.getInt(1),
        rs.getString(2),
        rs.getInt(3)
      ) 

      val topicId : String = rs.getString(4)

      graph += dependencyMetadata -> topicId

    } 

    ps.close()
    graph


  }

  /**
    * @method getTopicMetadata
    * @param topicId {String}
    * @return {TopicMetadata}
   **/
  private def getTopicMetadata(topicId : String) : TopicMetadata = { 

    val actionMetadata : ActionMetadata = getAbsoluteActionMetadata(topicId)
    val conditionMetadata : ConditionMetadata = getConditionMetadata(topicId) 
    
    TopicMetadata(topicId, conditionMetadata, actionMetadata) 
    
  }

  /**
    * @method unsimplifiedConditions
    * @param conditionMetadata {ConditionMetadata}
    * @return {Iterable[Condition]}
   **/
  private def unsimplifiedConditions(conditionMetadata : ConditionMetadata) : Iterable[Condition] = for { 
    (conditionId, conditionValue) <- conditionMetadata
  } yield { 
    val actions : List[Action] = unsimplifiedActions(getConditionalActionMetadata(conditionId)).toList
    Condition(conditionValue, actions)
  } 

  /** 
    * @method unsimplifiedActions 
    * @param actionMetadata {ActionMetadata} 
    * @return {Iterable[Action]}
   **/
  private def unsimplifiedActions(actionMetadata : ActionMetadata) : Iterable[Action] = for {
    (actionId, actionValue) <- actionMetadata
  } yield {

    val dependencies : List[Dependency] = (getDependencyGraph(actionId) map { 
        tuple : (DependencyMetadata, String)  => { 
          Dependency(tuple._1.value, List(Topic(tuple._2, List.empty[Term])))
        } 
    }).toList

    Action(actionValue, dependencies)

  }

  /**
    * @method unsimplifiedTerms
    * @param topicsMetadata {TopicMetadata} 
    * @return {Iterable[Term]}
   **/
  private def unsimplifiedTerms(topicMetadata : TopicMetadata) : Iterable[Term] = { 

    val a : ActionMetadata = topicMetadata.actionMetadata
    val c : ConditionMetadata = topicMetadata.conditionMetadata

    unsimplifiedActions(a) ++ unsimplifiedConditions(c) 

  }

  /**
    * @method unsimplifiedTopics
    * @return {Iterable[Topic]}
   **/
  private def unsimplifiedTopics() : Iterable[Topic] = {

    val topicList : List[String] = getTopicList

    val topicsMetadata : List[TopicMetadata] = topicList map { 
      getTopicMetadata(_)
    }

    for { 
      topicMetadata <- topicsMetadata 
    } yield Topic(topicMetadata.value, unsimplifiedTerms(topicMetadata).toList)

  } 

  /**
    * @method load
    * @return {Environment}
   **/
  def load() : Environment = {

    val env : Environment = new Environment
    
    env.insertTopics(unsimplifiedTopics.toList) // TODO: Make sure all terms are simplified
 
    env 
  }


} 
