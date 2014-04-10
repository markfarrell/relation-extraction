package edu.berkeley.crea.syntax

import java.util.Calendar

import java.io.StringWriter

import scala.util.Random

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
import it.uniroma1.dis.wsngroup.gexf4j.core.Edge
import it.uniroma1.dis.wsngroup.gexf4j.core.data.Attribute
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeClass
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeList
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeType
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.GexfImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.data.AttributeListImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.viz.NodeShape
import it.uniroma1.dis.wsngroup.gexf4j.core.viz.Color
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.viz.ColorImpl

import TreeConversions._

/** 
  * @class Environment
  * An environment stores and updates 
  * information as sentences are inserted 
  * into it. 
 **/
class Environment {

  import Environment.Term
  import Environment.Topic
  import Environment.Condition
  import Environment.Action
  import Environment.Dependency

  private var topicMap : Map[String, Topic] = Map.empty
  private var currentSize : Int = 0 // Number of unique terms inserted so far.

  /**
    * @method size
    * @return - Number of unique terms in the environment.
   **/
  def size() : Int = currentSize

  /** 
    * @method insertTopics
    * @param topics - The list of topics to be loaded 
    * into the environment. 
    * @param shouldRecolor - Optionally specify whether or not to recolor terms 
    * at all. 
    * @param color - The call-by-name color to perform recoloring operations with. 
    * @post - currentColor is rechosen before this method returns. The
    * currentSize of the is also updated.
   **/
  def insertTopics(topics : List[Topic], shouldRecolor : Boolean = true, color : => Color = Environment.nextColor()) : Unit = { 
    
    def recolor(topic : Topic) : Topic = recolorTopic(topic, color) 
   
    val recoloredTopics : List[Topic] = if(shouldRecolor) { 
      topics map recolor
    } else { 
      topics
    } 
    
    for(topic <- recoloredTopics) {

      topicMap.get(topic.value) match { 
        case Some(existingTopic) => {

          val updatedTopic : Topic = Topic(
            topic.value,
            mergeTerms(topic.abilities, existingTopic.abilities), 
            topic.color
          )

          topicMap += topic.value -> updatedTopic

        } 
        case None => { 
          currentSize += 1
          topicMap += topic.value -> topic
        } 
      }

      def insertActions(actions : List[Action]) : Unit = for { 
        action <- actions 
      } { 
       currentSize += 1
       insertDependencies(action.dependencies)
      } 

      def insertConditions(conditions : List[Condition]) : Unit = for { 
        condition <- conditions
      } { 
        currentSize += 1
        insertActions(condition.actions) 
      } 

      def insertDependencies(dependencies : List[Dependency]) : Unit = for { 
        dependency <- dependencies
      } { 
       currentSize += 1
       insertTopics(dependency.clauses, color = topic.color)
      } 

      insertConditions(getConditions(topic.abilities))
      insertActions(getActions(topic.abilities))

    }

  }  

  /** 
    * @method selectTopics
    * @return - All topics found in the environment, where 
    * topics inserted with the same values have been merged together. 
    **/
  def selectTopics() : List[Topic] = {

    def selectActions(terms : List[Term]) : List[Action]  = for { 
      action <- getActions(terms)
    } yield Action(
      action.value,
      selectDependencies(action.dependencies),
      action.color
    )

    def selectConditions(terms : List[Term]) = for { 
      condition <- getConditions(terms)
    } yield Condition(
      condition.value, 
      selectActions(condition.actions), 
      condition.color
    )

    def selectDependencies(dependencies : List[Dependency]) : List[Dependency] = for { 
      dependency <- dependencies
    } yield Dependency(
      dependency.value, 
      dependency.clauses map { 
        (t : Topic) => topicMap.get(t.value)
      } flatten,
      dependency.color
    )
  
    { 
      for { 
        topic <- topicMap.values 
      } yield Topic ( 
        topic.value,
        selectActions(topic.abilities) ++ selectConditions(topic.abilities),
        topic.color
      )
    } toList

  }

  /** 
    * @method getActions
    * @param terms {List[Term} 
    * @return {List[Action]}
   **/
  private def getActions(terms : List[Term]) : List[Action] = terms filter { 
    _.isInstanceOf[Action]
  } map { 
    _.asInstanceOf[Action] 
  }

  /**
    * @method getConditions
    * @param terms {List[Terms]}
    * @return {List[Condition]}
   **/
  private def getConditions(terms : List[Term]) : List[Condition]  = terms filter { // type select 
    _.isInstanceOf[Condition]
  } map { 
    _.asInstanceOf[Condition]
  }

  /**
    * @method mergeTerms
    * @param terms {List[Term]}
    * @return {List[Term]}
   **/
  private def mergeTerms(existingTerms : List[Term], newTerms : List[Term]) : List[Term] = {

    val terms : List[Term] = newTerms ++ existingTerms

    def mergeDependencies(dependencies : List[Dependency]) : List[Dependency]  = {

      var map : Map[String, Dependency] = Map.empty

      for(dependency <- dependencies) {
        map.get(dependency.value) match { 
          case Some(d) => {
            map += d.value -> Dependency(d.value, 
              d.clauses ++ dependency.clauses,
              dependency.color
            )
          } 
          case None => map += dependency.value -> dependency
        }
      } 

      map.values.toList 
    }

    def mergeActions(actions : List[Action]) : List[Action] = {

      var map : Map[String, Action] = Map.empty

      for(action <- actions) {
        map.get(action.value) match { 
          case Some(a) => {
            map += a.value -> Action(
              a.value, 
              mergeDependencies(a.dependencies ++ action.dependencies),
              action.color
            )
          } 
          case None => map += action.value -> action
        }
      } 

      map.values.toList 

    }

    def mergeConditions(conditions : List[Condition]) : List[Condition] = {

      var map : Map[String, Condition] = Map.empty

      for(condition <- conditions) {
        map.get(condition.value) match { 
          case Some(c) => {
            map += c.value -> Condition(
              c.value, 
              mergeActions(c.actions ++ condition.actions),
              condition.color
            )
          } 
          case None => map += condition.value -> condition
        }
      } 

      map.values.toList 

    } 

    mergeActions(getActions(terms)) ++ mergeConditions(getConditions(terms))

  }

  /** 
    * @method recolor - Changes the colors of each topic, as well as each of 
    * its conditions, actions, and subsequent dependencies.
    * @param topics {Topic} 
    * @param color {Color} 
    * @return {Topic} - The recolored list of Topics. 
   **/
  private def recolorTopic(topic : Topic, color : Color) : Topic = { 

    def recolorConditions(conditions : List[Condition]) = for { 
      condition <- conditions
    } yield Condition(  
      condition.value,
      recolorActions(condition.actions), 
      color 
    ) 

    def recolorActions(actions : List[Action]) = for { 
      action <- actions 
    } yield Action(
      action.value,
      recolorDependencies(action.dependencies),
      color 
    )  

    def recolorDependencies(dependencies : List[Dependency]) = for { 
      dependency <- dependencies
    } yield Dependency(  
      dependency.value,
      dependency.clauses,
      color 
    )

    val recoloredActions : List[Action] = recolorActions(getActions(topic.abilities)) 
    val recoloredConditions : List[Condition] = recolorConditions(getConditions(topic.abilities))
 
    val recoloredAbilities : List[Term] = recoloredActions ++ recoloredConditions
    Topic(topic.value, recoloredAbilities, color) 

  } 

} 

object Environment { 

  // Set to false to make nextColor 
  var randomize : Boolean = true 

  private val random : Random = new Random()
  private val defaultColor : Color = new ColorImpl(0,0,0)

  private val conditionTags : Set[String] = Set[String]("MD")
  private val dependencyTags : Set[String] = Set[String]("IN")
  private val clauseTags : Set[String] = Set[String]("S", "SBAR")
  private val topicClauseTags : Set[String] = Set[String]("S") 
  private val vpTags : Set[String] = Set[String]("VP")
  private val topicTags : Set[String] = Set[String]("NP", "NNS", "NNP", "NNPS", "CC")
  private val actionTags : Set[String] = Set[String]("VB", "VBZ", "VBP", "VBD", "CC")
  
  abstract class Term { 
    def color : Color;  
  }

  case class Action(value : String, dependencies : List[Dependency],
    color : Color = defaultColor) extends Term // VB, VBZ, VBP, VBD, CC

  case class Dependency(value : String, clauses: List[Topic],
    color : Color = defaultColor) extends Term

  case class Condition(value : String, actions : List[Action],
    color : Color = defaultColor) extends Term

  case class Topic(value : String, abilities : List[Term],
    color : Color = defaultColor) extends Term //VGD, NP, CC

 /** 
   * @method toTopic
   * @param tree
  **/
  def toTopic(tree : LinguisticTree) : Option[Topic] = tree.findCut(topicTags) map { 
    (t : LinguisticTree) => Topic(t.terminalList().mkString(" ").toLowerCase, toCondition(tree).orElse(toAction(tree)) match { 
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
    * @param term
   **/
  def toGexf(terms : List[Term]) : Gexf = {

    val gexf : Gexf = new GexfImpl()

    gexf.setVisualization(true)

    val graph : Graph = gexf.getGraph()
    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    val attrList : AttributeList = new AttributeListImpl(AttributeClass.NODE)
    graph.getAttributeLists().add(attrList)

    val attType : Attribute = attrList.createAttribute("type", AttributeType.STRING, "type")

    var currentNodeId : Int = 0 // Current node ID 
    var currentEdgeId : Int = 0 // Current edge ID 

    // Map of already visited topic nodes
    var visitedTopics : Map[String, Node] = Map.empty[String, Node] 

    // Map of already visited dependencies, to make two different actions
    // point to the same dependency. 
    var visitedDependencies : Map[String, Node] = Map.empty[String, Node]

    def makeNode(typename : String, value : String, terms : List[Term]) : Node = { 

      val nodeId : Int = currentNodeId
      
      currentNodeId += 1

      val node : Node = graph.createNode(nodeId.toString).setLabel(value) // Look for or get if existing 

      node.getAttributeValues().addValue(attType, typename)

      for(term <- terms) {

        val edgeId : Int = currentEdgeId
        currentEdgeId += 1

        val edge : Edge = node.connectTo(edgeId.toString, toNode(term))

        edge.setColor(term.color) 
      } 

      node

    } 

    def toNode(term : Term) : Node = {

      term match { 
        case Topic(value, abilities, color) => visitedTopics.get(value).getOrElse({ 
          val node : Node = makeNode("Topic", value, abilities)
          visitedTopics += value -> node
          node
        })
        case Dependency(value, clauses, color) => {

          // Test for equality by summing dependency value + all topic values
          val key : String = { 
            val strings : List[String] = value :: (clauses map { _.value })
            strings.mkString("")
          } 

          visitedDependencies.get(key).getOrElse({
            val node : Node = makeNode("Dependency", value, clauses)
            visitedDependencies += key -> node
            node 
          })
        } 
        case Condition(value, actions, color) => makeNode("Condition", value, actions)
        case Action(value, dependencies, color) => { 
          makeNode("Action", value, dependencies) 
        } 
      }

    }
    
    for (term <- terms) yield toNode(term) 
    
    gexf

  }

  /** 
    * @method nextColor - Generates a random color. 
    * @return {Color} - A random color. 
   **/
  private def nextColor() : Color = if(randomize) { 

    val rgb : Tuple3[Int, Int, Int] = (
      random.nextInt(256),
      random.nextInt(256), 
      random.nextInt(256)
    ) 

    new ColorImpl(rgb._1, rgb._2, rgb._3) 

  } else defaultColor

} 
 
