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

import it.uniroma1.dis.wsngroup.gexf4j.core.viz.Color

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
  private case class Metadata(id : Int, value : String, color : Color) // For topics, actions, conditions, dependencies ...
  private case class TopicBuilder(value : String, conditions : List[Metadata], actions : List[Metadata]) 

  /** 
    * @method selectTopics
    * @return {PreparedStatement}
   **/
  private def selectTopics() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.topics")

  /**
    * @method selectedAbsoluteActions
    * @return {PreparedStatement}
   **/
  private def selectAbsoluteActions() : PreparedStatement  = conn.prepareStatement("SELECT * FROM beagle.actions WHERE topic_value = ?")

  /**
    * @method selectConditionalActions
    * @return {PreparedStatement}
   **/
  private def selectConditionalActions() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.actions WHERE condition_id = ?")

  /**
    * @method selectConditions
    * @return {PreparedStatement} 
   **/
  private def selectConditions() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.conditions WHERE topic_value = ?") 

  /**
    * @method selectDependencies
    * @return {PreparedStatement} 
   **/
  private def selectDependencies() : PreparedStatement = conn.prepareStatement("SELECT * FROM beagle.dependencies WHERE action_id = ?") 


  /**
    * @method fetchMetadata {T extends Metadata}
    * @param ps {PreparedStatement}
    * @return {List[Metadata]}
   **/
  private def fetchMetadata(ps : PreparedStatement) : List[Metadata] = { 

    val rs : ResultSet = ps.executeQuery()
    var lst : List[Metadata] = List.empty[Metadata]

    while(rs.next()) {

      val id : Int = rs.getInt(1) 
      val value : String = rs.getString(2) 
      val color : Color = Colors.obj(rs.getInt(3))
      val metadata : Metadata = Metadata(id, value, color) 

      lst = metadata :: lst

    } 

    ps.close()

    lst

  } 

  /**
    * @method getTopicList
    * @return {List[Metadata]} 
   **/
  private def getTopicList() : List[Metadata] = fetchMetadata(selectTopics())

  /**
    * @method getAbsoluteActionMetadata
    * @param topicId 
    * @return {List[Metadata]} 
   **/
  private def getAbsoluteActionMetadata(topicValue : String) : List[Metadata] = {

    val ps : PreparedStatement = selectAbsoluteActions()
    ps.setString(2, topicValue)
    
    fetchMetadata(ps) 
    
  }

  /**
    * @method getConditionalActionMetadata
    * @param conditionId {Int}
    * @return {List[Metadata]}
   **/
  private def getConditionalActionMetadata(conditionId : Int) : List[Metadata] = { 

    val ps : PreparedStatement = selectConditionalActions()
    ps.setInt(1, conditionId)

    fetchMetadata(ps) 

  } 

  /** 
    * @method getConditionMetadata
    * @param topicId {String}
    * @return {ConditionMetadata}
   **/
  private def getConditionMetadata(topicId : String) : ConditionMetadata = { 

    val ps : PreparedStatement = selectConditions()
    ps.setString(1, topicId) 

    fetchMetadata(ps) 

  }

  /**
    * @method getTopicBuilder
    * @param topicValue {String}
    * @return {TopicBuilder}
   **/
  private def getTopicBuilder(topicValue : String) : TopicBuilder = { 

    val actionMetadata : List[Metadata] = getAbsoluteActionMetadata(topicValue)
    val conditionMetadata : List[Metadata] = getConditionMetadata(topicValue) 
    
    TopicBuilder(topicValue, conditionMetadata, actionMetadata) 
    
  }

  /**
    * @method unsimplifiedDependencies
    * @param actionId {Int} 
    * @return {List[Dependency]}
   **/
  private def unsimplifiedDependencies(actionId : Int) : List[Dependency] = {

    var dependencies : List[Dependency] = List.empty[Dependency] 

    val ps : PreparedStatement = selectDependencies()
    ps.setInt(1, actionId)

    val rs : ResultSet = ps.executeQuery()

    while(rs.next()) {

      val id : Int = rs.getInt(1)
      val value : String = rs.getString(2) 
      val color : Color = Colors.obj(rs.getInt(3))
      val metadata : Metadata = Metadata(id, value, color) 

      val actionId : Int = rs.getInt(4) 
      val topicValue : String = rs.getString(5)

      val topic : Topic = Topic(topicValue, List.empty[Term]) 

      dependencies = Dependency(metadata.value, List(topic)) :: dependencies

    } 

    ps.close()

    dependencies 
  }

  /**
    * @method unsimplifiedConditions
    * @param conditionMetadata {ConditionMetadata}
    * @return {Iterable[Condition]}
   **/
  private def unsimplifiedConditions(conditionMetadata : List[Metadata]) : Iterable[Condition] = for { 
    metadata <- conditionMetadata
  } yield { 
    val actions : List[Action] = unsimplifiedActions(getConditionalActionMetadata(metadata.id)).toList
    Condition(metadata.value, actions)
  } 

  /** 
    * @method unsimplifiedActions 
    * @param actionMetadata {ActionMetadata} 
    * @return {Iterable[Action]}
   **/
  private def unsimplifiedActions(actionMetadata : List[Metadata]) : Iterable[Action] = for {
    metadata <- actionMetadata
  } yield {

    val actionId : Int = metadata.id

    Action(actionValue, unsimplifiedDependencies(actionId))

  }

  /**
    * @method unsimplifiedTerms
    * @param topicBuilder {TopicBuilder} 
    * @return {Iterable[Term]}
   **/
  private def unsimplifiedTerms(topicBuilder : TopicBuilder) : Iterable[Term] = { 

    val a : List[Metadata] = topicBuilder.actionMetadata
    val c : List[Metadata] = topicBuilder.conditionMetadata

    unsimplifiedActions(a) ++ unsimplifiedConditions(c) 

  }

  /**
    * @method unsimplifiedTopics
    * @return {Iterable[Topic]}
   **/
  private def unsimplifiedTopics() : Iterable[Topic] = {

    val topicList : List[String] = getTopicList

    val topicBuilders : List[TopicBuilder] = topicList map { 
      getTopicBuilder(_)
    }

    for { 
      topicBuilder <- topicBuilders
    } yield Topic(topicBuilder.value, unsimplifiedTerms(topicBuilder).toList)

  } 

  /**
    * @method load
    * @return {Environment}
   **/
  def load() : Environment = {

    val env : Environment = new Environment
    
    env.insertTopics(unsimplifiedTopics.toList) 
 
    env 
  }


} 
