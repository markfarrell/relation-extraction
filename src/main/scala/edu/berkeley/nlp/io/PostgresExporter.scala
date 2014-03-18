package edu.berkeley.nlp.io 

import java.sql.DriverManager
import java.sql.Connection
import java.sql.Statement
import java.sql.PreparedStatement
import java.sql.SQLException
import java.sql.Types
import java.sql.ResultSet
import java.util.Properties

import scala.collection.mutable.{HashMap, MultiMap, Set}

import edu.berkeley.nlp.syntax.Tree

import edu.berkeley.nlp.syntax.TreeConversions._
import edu.berkeley.nlp.syntax.Environment
import edu.berkeley.nlp.syntax.Environment.Term
import edu.berkeley.nlp.syntax.Environment.Topic
import edu.berkeley.nlp.syntax.Environment.Dependency
import edu.berkeley.nlp.syntax.Environment.Condition
import edu.berkeley.nlp.syntax.Environment.Action

/** 
  * @class PostgresExporter
  * 
  * @constructor Export an environment to a PostgreSQL database.
  * @param conn - active database connection.
  * @param env - environment loaded with terms. 
  **/
class PostgresExporter(conn : Connection, env : Environment) { 

  class DependenciesTable extends HashMap[Dependency, Set[Action]] with MultiMap[Dependency, Action]
  class TopicsTable extends HashMap[Term, Set[Topic]] with MultiMap[Term, Topic]
  class ConditionsTable extends HashMap[Action, Set[Condition]] with MultiMap[Action, Condition]

  private val topics : List[Topic] = env.selectTopics()

  private val topicsTable : TopicsTable = new TopicsTable 
  private val conditionsTable : ConditionsTable = new ConditionsTable
  private val dependenciesTable : DependenciesTable = new DependenciesTable

  private val yes: Int = Statement.RETURN_GENERATED_KEYS

  /** 
    * @method insertTopic
    * @return A new prepared statement for the insertion of topics. 
   **/
  private def insertTopic : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.topics VALUES(?)")
  
  /**
    * @method insertAction
    * @return A new prepared statement for the insertion of actions.
   **/
  private def insertAction : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.actions VALUES(DEFAULT, ?,?,?)", yes)

  /**
    * @method insertCondition
    * @return A new prepared statement for the insertion of conditions.
   **/
  private def insertCondition : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.conditions VALUES(DEFAULT, ?,?)", yes)

  /** 
    * @method insertDependency
    * @return A new prepared statement for the insertion of dependencies. 
   **/ 
  private def insertDependency : PreparedStatement = conn.prepareStatement("INSERT INTO beagle.dependencies VALUES(DEFAULT, ?,?,?)")

  /** 
    * @method zipKeys
    * @param prepareStatement - A prepareStatement to a get a result of generated keys for 
    * after making insertions into a table.
    * @param terms - A list of terms to associate each generated ID with.
    * @return An immutable map containing (unique generated ID, term) pairs.
   **/ 
  private def zipKeys[A <: Term](preparedStatement : PreparedStatement, terms : List[A]) : Map[Int, A] = { 

    var ids : Map[Int, A] = Map.empty[Int, A]
    val rs : ResultSet = preparedStatement.getGeneratedKeys()
    var i : Int = 0 

    assert(rs != null)

    while( rs.next() ) { 
      ids += rs.getInt(1) -> terms(i)
      i = i+1  
    }

    assert(ids.keys.size == terms.size, ids.keys.size + "==" + terms.size) 

    ids

  }

  /**
    * @method pluckKeys 
    * @param terms - Terms to retrieve an iterable of primary keys for. 
    * @param termsTable - A table of keys corresponding to a certain terms. Note that the table
    * is comprised of (Int, Term) pairs rather than (Term, Int) pairs because terms are not necessarily unique.
    * @return - A set of keys corresponding each term in the set of terms passed.
    **/
  private def pluckKeys[A <: Term](terms : Set[A], termsTable : Map[Int, A]) : Iterable[Int] = { 
    //assert(terms.size == termsTable.size, terms.size + "==" + termsTable.size) 
    termsTable filter { 
      kv : (Int, A)  => terms contains { kv._2 }
    } keys
  } 

  /** 
    * @method extractDependencies - Adds a key-value entry to the dependencies
    * table for each dependenchy that the action has.
    * @param action 
   **/
  private def extractDependencies(action : Action) : Unit = for (dependency <- action.dependencies) { 
    dependenciesTable.addBinding(dependency, action) 
  }

  /**
    * @method extract - Populates tables of many-to-one mappings
    * for terms in the environment, preparing export. 
   **/
  private def extract() : Unit = for (topic <- topics) {

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

  /**
    * @method exportTopics
   **/
  private def exportTopics() : Unit = {

    val insertedTopic : PreparedStatement = insertTopic

    for(topic <- topics) { 
      insertedTopic.setString(1, topic.value)
      insertedTopic.addBatch() // Add current parameters to batch
      insertedTopic.clearParameters()
    } 

    insertedTopic.executeBatch()

    insertedTopic.close()

  }

  /**
    * @method exportAbsoluteActions
    * @return - A map of primary key -> action pairs.
   **/
  private def exportAbsoluteActions() : Map[Int, Action] = { 

    var ids : Map[Int, Action] = Map.empty[Int, Action] 

    val insertedAction : PreparedStatement = insertAction

    for(kv <- topicsTable.iterator) {
      kv._1 match { 
        case action : Action => { 
          for(topic <- kv._2) { 

            insertedAction.setString(1, action.value)
            insertedAction.setNull(2, Types.INTEGER)
            insertedAction.setString(3, topic.value)
            insertedAction.executeUpdate() 
            insertedAction.clearParameters()

            ids = ids ++ zipKeys[Action](insertedAction, List[Action](action))

          }
        }
        case _ => {} 
      }
    }

    insertedAction.close()
    ids // Produce ID table

  }

  /**
    * @method exportConditions
    * @return - A map of primary key -> condition pairs.
   **/
  private def exportConditions() : Map[Int, Condition] = {

    var ids : Map[Int, Condition] = Map.empty[Int, Condition] 

    val insertedCondition : PreparedStatement = insertCondition

    for(kv <- topicsTable.iterator) {
      kv._1 match { 
        case condition : Condition => {
          for (topic <- kv._2) {

            insertedCondition.setString(1, condition.modal) 
            insertedCondition.setString(2, topic.value) 
            insertedCondition.executeUpdate()
            insertedCondition.clearParameters()

            ids = ids ++ zipKeys[Condition](insertedCondition, List[Condition](condition))

          } 
        } 
      }

    }

    insertedCondition.close()
    ids

  }

  /**
    * @method exportConditionalActions
    * @return - A map of primary key -> action pairs.
   **/
  private def exportConditionalActions(conditionsIds : Map[Int, Condition]) : Map[Int, Action] = {

    var ids : Map[Int, Action] = Map.empty[Int, Action] 

    val insertedAction : PreparedStatement = insertAction

    for(kv <- conditionsTable) { 

      val action : Action = kv._1

      for (conditionId <- pluckKeys[Condition](kv._2, conditionsIds)) { 

        insertedAction.setString(1, action.value) 
        insertedAction.setInt(2, conditionId) // Set condition ID 
        insertedAction.setNull(3, Types.VARCHAR) 
        insertedAction.executeUpdate()
        insertedAction.clearParameters()

        ids = ids ++ zipKeys[Action](insertedAction, List[Action](action))

      } 

    } 

    insertedAction.close()
    ids

  }

  /**
    * @method exportDependencies
   **/
  private def exportDependencies(actionsIds : Map[Int, Action]) : Unit = {
    val insertedDependency : PreparedStatement = insertDependency

    for( kv <- dependenciesTable ) { 

      val dependency : Dependency = kv._1 

      for(actionId <- pluckKeys[Action](kv._2, actionsIds)) {

        for(topic <- dependency.clauses) { 

          insertedDependency.setString(1, dependency.value) 
          insertedDependency.setInt(2, actionId)
          insertedDependency.setString(3, topic.value)
          insertedDependency.executeUpdate()
          insertedDependency.clearParameters()

        } 

      } 
    }

    insertedDependency.close()

  }

  /** 
    * @method export - Exports terms loaded in the environment passed
    * as constructor argument to the database.
   **/
  def export() : Unit = {

    extract()

    try { 

      var actionsIds : Map[Int, Action] = Map.empty[Int, Action]
      var conditionsIds : Map[Int, Condition] = Map.empty[Int, Condition]

      // Commit all or nothing, since we want to enforce the same relationship 
      // between terms in the environment. 
      conn.setAutoCommit(false) 

      // Insert all new topics into the database 
      // TODO: What happens if a topic already exists?
      exportTopics()     

      // Insert new actions that point to a topic
      actionsIds = exportAbsoluteActions()

      // Insert new conditions that point to a topic
      conditionsIds = exportConditions()  

      // Grab all generated condition keys
      // Insert new actions that point to a condition
      actionsIds = actionsIds ++ exportConditionalActions(conditionsIds)

      // Grab all generated action keys
      // Insert dependencies that point to actions
      exportDependencies(actionsIds)   

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


