package edu.berkeley.nlp.syntax

import java.util.Calendar

import java.io.StringWriter 

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.immutable.Map

import it.uniroma1.dis.wsngroup.gexf4j.core.EdgeType
import it.uniroma1.dis.wsngroup.gexf4j.core.Gexf
import it.uniroma1.dis.wsngroup.gexf4j.core.Graph
import it.uniroma1.dis.wsngroup.gexf4j.core.Mode
import it.uniroma1.dis.wsngroup.gexf4j.core.Node
import it.uniroma1.dis.wsngroup.gexf4j.core.data.Attribute
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeClass
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeList
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeType
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.GexfImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.data.AttributeListImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.viz.NodeShape

import TreeConversions._

/** 
  * @class Environment
  * An environment stores and updates 
  * information as sentences are inserted 
  * into it. 
 **/
class Environment {

  private var topicMap : Map[String, Environment.Topic] = Map.empty
  
  private var currentSize : Int = 0 // Statistic about the number of unique terms inserted so far. 
  
  /**
    * @method size
    * @return - Number of unique terms in the environment.
   **/
  def size() : Int = currentSize

  /** 
    * @method insertTopics
    * @param topics - The list of topics to be loaded 
    * into the environment. 
   **/
  def insertTopics(topics : List[Environment.Topic]) : Unit = for(topic <- topics) {

    topicMap.get(topic.value) match { 
      case Some(existingTopic) => { 

        //Note: will contain duplicate actions e.g.  "runs ... case 1", "runs ... case 2"
        topicMap += topic.value -> Environment.Topic(topic.value, mergeTerms(existingTopic.abilities ++ topic.abilities)) 

      } 
      case None => { 
        currentSize += 1
        topicMap += topic.value -> topic 
      } 
    }

    def insertActions(actions : List[Environment.Action]) : Unit = for { 
      action <- actions 
    } { 
      currentSize += 1
      insertDependencies(action.dependencies)
    } 

    def insertConditions(conditions : List[Environment.Condition]) : Unit = for { 
      condition <- conditions
    } { 
      currentSize += 1
      insertActions(condition.actions) 
    } 

    def insertDependencies(dependencies : List[Environment.Dependency]) : Unit = for { 
      dependency <- dependencies
    } { 
      currentSize += 1
      insertTopics(dependency.clauses)
    } 

    insertConditions(getConditions(topic.abilities))

    insertActions(getActions(topic.abilities))
     
  } 

  /** 
    * @method selectTopics
    * @return - All topics found in the environment, where 
    * topics inserted with the same values have been merged together. 
    **/
  def selectTopics() : List[Environment.Topic] = {

    def selectActions(terms : List[Environment.Term]) : List[Environment.Action]  = for { 
      action <- getActions(terms)
    } yield Environment.Action(action.value, selectDependencies(action.dependencies))

    def selectConditions(terms : List[Environment.Term]) = for { 
      condition <- getConditions(terms)
    } yield Environment.Condition(condition.modal, selectActions(condition.actions))

    def selectDependencies(dependencies : List[Environment.Dependency]) : List[Environment.Dependency] = for { 
      dependency <- dependencies
    } yield Environment.Dependency(dependency.value, dependency.clauses map { 
        (t : Environment.Topic) => topicMap.get(t.value)
      } flatten)
  
    { 
      for { 
        topic <- topicMap.values 
      } yield Environment.Topic(topic.value, selectActions(topic.abilities) ++ selectConditions(topic.abilities))
    } toList

  }

  /** 
    * @method getActions
    * @param terms {List[Environment.Term} 
    * @return {List[Environment.Action]}
   **/
  private def getActions(terms : List[Environment.Term]) : List[Environment.Action] = terms filter { 
    _.isInstanceOf[Environment.Action]
  } map { 
    _.asInstanceOf[Environment.Action] 
  }

  /**
    * @method getConditions
    * @param terms {List[Environment.Terms]}
    * @return {List[Environment.Condition]}
   **/
  private def getConditions(terms : List[Environment.Term]) : List[Environment.Condition]  = terms filter { // type select 
    _.isInstanceOf[Environment.Condition]
  } map { 
    _.asInstanceOf[Environment.Condition]
  }

  /**
    * @method mergeTerms
    * @param terms {List[Environment.Term]}
    * @return {List[Environment.Term]}
  **/
  private def mergeTerms(terms : List[Environment.Term]) : List[Environment.Term] = { 

    def mergeDependencies(dependencies : List[Environment.Dependency]) : List[Environment.Dependency]  = {

      var map : Map[String, Environment.Dependency] = Map.empty

      for(dependency <- dependencies) {
        map.get(dependency.value) match { 
          case Some(d) => {
            map += d.value -> Environment.Dependency(d.value, d.clauses ++ dependency.clauses)
          } 
          case None => map += dependency.value -> dependency
        }
      } 

      map.values.toList 
    }

    def mergeActions(actions : List[Environment.Action]) : List[Environment.Action] = {

      var map : Map[String, Environment.Action] = Map.empty

      for(action <- actions) {
        map.get(action.value) match { 
          case Some(a) => {
            map += a.value -> Environment.Action(a.value, mergeDependencies(a.dependencies ++ action.dependencies))
          } 
          case None => map += action.value -> action
        }
      } 

      map.values.toList 

    }

    def mergeConditions(conditions : List[Environment.Condition]) : List[Environment.Condition] = {

      var map : Map[String, Environment.Condition] = Map.empty

      for(condition <- conditions) {
        map.get(condition.modal) match { 
          case Some(c) => {
            map += c.modal -> Environment.Condition(c.modal, mergeActions(c.actions ++ condition.actions))
          } 
          case None => map += condition.modal -> condition
        }
      } 

      map.values.toList 

    } 

    mergeActions(getActions(terms)) ++ mergeConditions(getConditions(terms))

  }

} 

object Environment { 

  val conditionTags : Set[String] = Set[String]("MD")
  val dependencyTags : Set[String] = Set[String]("IN")
  val clauseTags : Set[String] = Set[String]("S", "SBAR")
  val topicClauseTags : Set[String] = Set[String]("S") 
  val vpTags : Set[String] = Set[String]("VP")
  val topicTags : Set[String] = Set[String]("NP", "CC")
  val actionTags : Set[String] = Set[String]("VB", "VBZ", "VBP", "VBD", "CC")

  abstract class Term
  case class Action(value : String, dependencies : List[Dependency]) extends Term // VB, VBZ, VBP, VBD, CC
  case class Dependency(value : String, clauses: List[Topic]) extends Term
  case class Condition(modal : String, actions : List[Action]) extends Term
  case class Topic(value : String, abilities : List[Term]) extends Term //VGD, NP, CC 

 /** 
    * @method toTopic
    * @param tree
    **/
  def toTopic(tree : LinguisticTree) : Option[Topic] = tree.findCut(topicTags) map { 
    (t : LinguisticTree) => Topic(t.terminalList().mkString(" "), toCondition(tree).orElse(toAction(tree)) match { 
        case Some(term) => List(term)
        case None => List()
     })
  } 

  /**
    * @method toDependency
    * @param tree
   **/
  def toDependency(tree : LinguisticTree) : Option[Dependency] = tree.findCut(dependencyTags) map { 
    (t : LinguisticTree) => Dependency(t.terminalList().mkString(" "), tree.findCut(topicClauseTags) flatMap { toTopic(_) } match { 
      case Some(term) => List(term)
      case None => List()
    })
  }

  /**
    * @method toAction
    * @param tree
   **/
  def toAction(tree : LinguisticTree) : Option[Action] = for { 
    t1 <- tree.findCut(vpTags)
    t2 <- t1.findCut(actionTags)
    action <- Some(Action(t2.terminalList().mkString(" "), toDependency(t1) match {
      case Some(term) => List(term)
      case None => List()
    }))
  } yield action


  /**
    * @method toCondition
    * @param tree
   **/
  def toCondition(tree : LinguisticTree) : Option[Condition] = for { 
    t1 <- tree.findCut(vpTags)
    t2 <- t1.findCut(conditionTags) 
    condition <- Some(Condition(t2.terminalList().mkString(" "), toAction(t1) match { 
      case Some(term) => List(term)
      case None => List()
    }))
  } yield condition

  /**
    * @method toClause
    * @param tree
   **/
  def toClause(tree : LinguisticTree) : Option[Term] = tree.iterator().asScala find { 
    (t : LinguisticTree) => clauseTags contains { t.getLabel() } 
  } flatMap { 
    (t : LinguisticTree) => t.getLabel() match { 
      case "S" => toTopic(t)
      case "SBAR" => toDependency(t)
    } 
  } orElse(toTopic(tree))

  /**
    * @method toGexf
    * TODO: If two actions share the same dependency, there should only be one node produced
    * for that dependency. 
    * @param term
   **/
  def toGexf(terms : List[Term]) : Gexf = {

    val gexf : Gexf = new GexfImpl()

    gexf.setVisualization(true)

    val graph : Graph = gexf.getGraph()
    graph.setDefaultEdgeType(EdgeType.UNDIRECTED).setMode(Mode.STATIC)

    val attrList : AttributeList = new AttributeListImpl(AttributeClass.NODE)
    graph.getAttributeLists().add(attrList)

    val attType : Attribute = attrList.createAttribute("type", AttributeType.STRING, "type")

    var visited : Map[String, Node] = Map.empty //List of already visited topic nodes
  
    def makeNode(typename : String, value : String, terms : List[Term]) : Node  = { 

      val node : Node = graph.createNode(value).setLabel(value) // Look for or get if existing 

      node.getAttributeValues().addValue(attType, typename)

      for(term <- terms) node.connectTo(value, toNode(term))

      node

    } 

    def toNode(term : Term) : Node = {

      term match { 
        case Topic(value, abilities) => visited.get(value).getOrElse({ 
          val node : Node = makeNode("Topic", value, abilities)
          visited += value -> node
          node
        })
        case Dependency(value, clauses) => makeNode("Dependency", value, clauses)
        case Condition(modal, actions) => makeNode("Condition", modal, actions)
        case Action(value, dependencies) => makeNode("Action", value, dependencies)
      } 
    }
    
    terms map { 
      (term : Term) => toNode(term)
    } 
    
    gexf

  } 

} 
 
