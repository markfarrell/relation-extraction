package edu.berkeley.crea.io

import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.PreparedStatement
import java.sql.SQLException
import java.sql.Types
import java.sql.ResultSet
import java.util.Properties

import scala.collection.mutable.HashMap

import edu.berkeley.crea.syntax.Environment
import edu.berkeley.crea.syntax.Environment.Term
import edu.berkeley.crea.syntax.Environment.Topic
import edu.berkeley.crea.syntax.Environment.Dependency
import edu.berkeley.crea.syntax.Environment.Condition
import edu.berkeley.crea.syntax.Environment.Action

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
  private case class TopicBuilder(value : String, conditions : List[Metadata], actions : List[Metadata], color : Color) 

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
    * @pre The PreparedStatement, ps, has been parameterized and is ready to be iterated over. 
    * @post Closes the prepared statement ps.
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
    * @method getAbsoluteActionMetadata
    * @param topicId 
    * @return {List[Metadata]} 
   **/
  private def getAbsoluteActionMetadata(topicValue : String) : List[Metadata] = {

    val ps : PreparedStatement = selectAbsoluteActions()
    ps.setString(1, topicValue)
    
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
    * @return {List[Metadata]}
   **/
  private def getConditionMetadata(topicId : String) : List[Metadata] = { 

    val ps : PreparedStatement = selectConditions()
    ps.setString(1, topicId) 

    fetchMetadata(ps) 

  }

  /**
    * @method getTopicBuilder
    * @param topicMetadata {Metadata}
    * @return {TopicBuilder}
   **/
  private def getTopicBuilder(topicMetadata : Metadata) : TopicBuilder = { 

    val topicValue : String = topicMetadata.value
    val topicColor : Color = topicMetadata.color 

    val actionMetadata : List[Metadata] = getAbsoluteActionMetadata(topicValue)
    val conditionMetadata : List[Metadata] = getConditionMetadata(topicValue) 
    
    TopicBuilder(topicValue, conditionMetadata, actionMetadata, topicColor) 
    
  }

  /**
    * @method partDependencies
    * @param actionId {Int} 
    * @return {List[Dependency]}
   **/
  private def partDependencies(actionId : Int) : List[Dependency] = {

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
    * @method partConditions
    * @param conditionMetadata {ConditionMetadata}
    * @return {Iterable[Condition]}
   **/
  private def partConditions(conditionMetadata : List[Metadata]) : Iterable[Condition] = for { 
    metadata <- conditionMetadata
  } yield { 
    val actions : List[Action] = partActions(getConditionalActionMetadata(metadata.id)).toList
    Condition(metadata.value, actions)
  } 

  /** 
    * @method partActions 
    * @param actionMetadata {ActionMetadata} 
    * @return {Iterable[Action]}
   **/
  private def partActions(actionMetadata : List[Metadata]) : Iterable[Action] = for {
    a <- actionMetadata
  } yield Action(a.value, partDependencies(a.id))

  /**
    * @method partTerms
    * @param builder {TopicBuilder} 
    * @return {Iterable[Term]}
   **/
  private def partTerms(builder : TopicBuilder) : Iterable[Term] = { 
    partActions(builder.actions) ++ partConditions(builder.conditions) 
  }

  /**
    * @method partTopics
    * @return {Iterable[Topic]}
   **/
  private def partTopics() : Iterable[Topic] = {

    val topicMetadata : List[Metadata] = fetchMetadata(selectTopics()) 

    val topicBuilders : List[TopicBuilder] = topicMetadata map { 
      getTopicBuilder(_)
    }

    for { 
      topicBuilder <- topicBuilders
    } yield Topic(topicBuilder.value, partTerms(topicBuilder).toList)

  } 

  /**
    * @method load
    * @return {Environment}
   **/
  def load() : Environment = {

    val env : Environment = new Environment
    
    env.insertTopics(partTopics.toList) 
 
    env

  }


} 
