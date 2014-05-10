package edu.berkeley.crea.io 

import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.PreparedStatement
import java.sql.SQLException
import java.sql.Types
import java.sql.ResultSet
import java.sql.BatchUpdateException
import java.util.Properties

import scala.collection.mutable.{HashMap, MultiMap, Set}

import edu.berkeley.nlp.syntax.Tree

import edu.berkeley.crea.syntax.TreeConversions._
import edu.berkeley.crea.syntax.Environment
import edu.berkeley.crea.syntax.Environment.Term
import edu.berkeley.crea.syntax.Environment.Topic
import edu.berkeley.crea.syntax.Environment.Dependency
import edu.berkeley.crea.syntax.Environment.Condition
import edu.berkeley.crea.syntax.Environment.Action

/** 
  * @class PostgresExporter
  * 
  * @constructor Export an environment to a PostgreSQL database.
  * @param conn - active database connection.
  * @param env - environment loaded with terms. 
  **/
class PostgresExporter(conn : Connection, env : Environment) { 

  private val topics : List[Topic] = env.selectTopics()

  private val yes : Int = Statement.RETURN_GENERATED_KEYS

  /** 
    * @method insertTopic
    * @return A new prepared statement for the insertion of topics. 
   **/
  private def insertTopic : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.topics VALUES(DEFAULT,?,?)")
  
  /**
    * @method insertAction
    * @return A new prepared statement for the insertion of actions.
   **/
  private def insertAction : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.actions VALUES(DEFAULT, ?,?,?,?)", yes)

  /**
    * @method insertCondition
    * @return A new prepared statement for the insertion of conditions.
   **/
  private def insertCondition : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.conditions VALUES(DEFAULT, ?,?,?)", yes)

  /** 
    * @method insertDependency
    * @return A new prepared statement for the insertion of dependencies. 
   **/ 
  private def insertDependency : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.dependencies VALUES(DEFAULT, ?,?,?,?)")


  private def getGeneratedKey(preparedStatement : PreparedStatement) : Option[Int] = { 

    var ids : List[Int] = List.empty[Int]
    val rs : ResultSet = preparedStatement.getGeneratedKeys()

    if (rs == null) None else {

      while( rs.next() ) { 
        ids = rs.getInt(1) :: ids
      }

      if (ids.size != 1) None else { 
        Some(ids.head)
      } 

    } 

  }

  /**
    * @method extract 
   **/
  private def extract(terms : List[Term]) : List[(Term, Term)] = {

    var relation : List[(Term, Term)] = List.empty[(Term, Term)]

    for (term <- terms) {

      val arrows : List[Term] = term match { 
        case topic : Topic => topic.abilities
        case condition : Condition => condition.actions
        case action : Action =>  action.dependencies
        case _ => List.empty[Term]
      }

      relation = relation ++ arrows.map { _ -> term } ++ extract(arrows)  

    }

    relation

  }

  private def exportRelation(relation : List[(Term, Term)], target : Option[Term] = None, key : Option[Int] = None) : Unit = {

    val insertedTopic : PreparedStatement = insertTopic
    val insertedAction : PreparedStatement = insertAction
    val insertedCondition : PreparedStatement = insertCondition
    val insertedDependency : PreparedStatement = insertDependency

    for(pair <- relation) pair match { 
      case (action : Action, topic : Topic) => { 

        insertedAction.setString(1, action.value)
        insertedAction.setInt(2, Colors.hex(action.color))
        insertedAction.setNull(3, Types.INTEGER)
        insertedAction.setString(4, topic.value)
        insertedAction.executeUpdate() 
        insertedAction.clearParameters()

        val newRelation : List[(Term, Term)] = relation collect {
          case (d : Dependency, a : Action) => (d,a)
        } 

        exportRelation(newRelation, Some(action), getGeneratedKey(insertedAction))

      } 
      case (condition : Condition, topic : Topic) => {

        insertedCondition.setString(1, condition.value) 
        insertedCondition.setInt(2, Colors.hex(condition.color))
        insertedCondition.setString(3, topic.value) 
        insertedCondition.executeUpdate()
        insertedCondition.clearParameters()

        val newRelation : List[(Term, Term)] = relation collect { 
          case (a : Action, c : Condition) => (a,c) 
          case (d : Dependency, a : Action) => (d,a)
        } 

        exportRelation(newRelation, Some(condition),  getGeneratedKey(insertedCondition))

      }

      case (action : Action, condition : Condition) if Some(condition) == target => {

        key match { 
          case Some(conditionId) => { 

            insertedAction.setString(1, action.value) 
            insertedAction.setInt(2, Colors.hex(action.color))
            insertedAction.setInt(3, conditionId) // Set condition ID 
            insertedAction.setNull(4, Types.VARCHAR)
            insertedAction.executeUpdate()
            insertedAction.clearParameters()

            val newRelation : List[(Term, Term)] = relation collect { 
              case (d : Dependency, a : Action) => (d,a) 
            }

            exportRelation(newRelation, Some(action), getGeneratedKey(insertedAction))

          } 
          case None => Unit 
        } 

      }
      case (dependency : Dependency, action : Action) if Some(action) == target => {

        key match { 
          case Some(actionId) => { 
            for(topic <- dependency.topics.collect{case t : Topic => t}) { 

              insertedDependency.setString(1, dependency.value)
              insertedDependency.setInt(2, Colors.hex(dependency.color))
              insertedDependency.setInt(3, actionId)
              insertedDependency.setString(4, topic.value)
              insertedDependency.executeUpdate()
              insertedDependency.clearParameters()

            }
          } 
          case None => Unit
        } 

      } 
      case _ => Unit
    } 

    insertedTopic.close()
    insertedAction.close()
    insertedCondition.close()
    insertedDependency.close() 

  } 

  /**
    * @method exportTopics
   **/
  private def exportTopics() : Unit = {

    val insertedTopic : PreparedStatement = insertTopic 

    for(topic <- topics) { 
      insertedTopic.setString(1, topic.value)
      insertedTopic.setInt(2, Colors.hex(topic.color))
      insertedTopic.addBatch() // Add current parameters to batch
      insertedTopic.clearParameters()
    }

    try { 

      insertedTopic.executeBatch()

    } catch { 
      case e: BatchUpdateException => { 
        e.getNextException()  
      } 
    } 

    insertedTopic.close()

  }

  /** 
    * @method export - Exports terms loaded in the environment passed
    * as constructor argument to the database.
   **/
  def export() : Unit = {

    try { 

      // Commit all or nothing, since we want to enforce the same relationship 
      // between terms in the environment. 
      conn.setAutoCommit(false) 

      // Insert all new topics into the database 
      exportTopics()
      exportRelation(extract(topics))

      conn.commit() // Complete transaction

    } catch { 
      case e : SQLException => { 
        conn.rollback()
        e.printStackTrace()
      } 
    } finally { 
      conn.setAutoCommit(true)
    } 

  } 

} 


