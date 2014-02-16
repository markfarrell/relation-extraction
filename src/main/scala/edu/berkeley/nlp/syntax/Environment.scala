package edu.berkeley.nlp.syntax

import java.util.Calendar

import java.io.StringWriter 

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.List
import scala.collection.immutable.Set

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

object Environment { 

  val conditionTags : Set[String] = Set[String]("MD")
  val dependencyTags : Set[String] = Set[String]("IN")
  val clauseTags : Set[String] = Set[String]("S", "SBAR")
  val vpTags : Set[String] = Set[String]("VP")
  val topicTags : Set[String] = Set[String]("NP", "CC")
  val actionTags : Set[String] = Set[String]("VB", "VBZ", "VBP", "VBD", "CC")

  abstract class Term
  case class Action(value : String, dependencies : List[Dependency]) extends Term // VB, VBZ, VBP, VBD, CC
  case class Dependency(value : String, clauses: List[Topic]) extends Term
  case class Condition(modal : String, actions : List[Action]) extends Term
  case class Topic(value : String, abilities : List[Term]) extends Term //VGD, NP, CC 
  //TODO: Add support for multiple actions: e.g. The dog can walk and might run.
  // Possible expand a sentence before passing it to the environment.

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
    (t : LinguisticTree) => Dependency(t.terminalList().mkString(" "), toTopic(tree) match { 
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
    * @method merge -- Ensures the uniqueness of 
    * term values, building one to many relations 
    * where the same term value is encountered more
    * than once in the list of terms provided. 
    * @param terms
   **/
  def merge(terms : List[Term]) : List[Term] = {

    var topics : List[Topic] = List()
    var actions : List[Action] = List()
    var conditions : List[Condition] = List()
    var dependencies : List[Dependency] = List()

    for(term <- terms) { 
      term match { 
        case Topic(value, abilities) => topics find {  _.value == value } match { 
          case Some(topic) => { 
            topics = topics filterNot { _.value == value }
            topics = Topic(value, merge(topic.abilities ++ abilities)) :: topics
          } 
          case None => topics = Topic(value, abilities) :: topics
        } 
        case Action(value, dependencies) => actions find {  _.value == value } match { 
          case Some(action) => { 
            actions = actions filterNot { _.value == value } 
            actions = Action(value, merge(action.dependencies ++ dependencies) map { 
              _.asInstanceOf[Dependency]
            }) :: actions 
          } 
          case None => actions = Action(value, dependencies) :: actions
        } 
        case Condition(modal, actions) => conditions find { _.modal == modal } match { 
          case Some(condition) => { 
            conditions = conditions filterNot { _.modal == modal } 
            conditions = Condition(modal, merge(condition.actions ++ actions) map {
              _.asInstanceOf[Action]
            }) :: conditions
          } 
          case None => conditions = Condition(modal, actions) :: conditions
        } 
        case Dependency(value, clauses) => dependencies find { _.value == value } match { 
          case Some(dependency) => {
            val combinedClauses : List[Topic] = dependency.clauses ++ clauses
            val filteredTopics : List[Topic] = topics filter { 
              (t : Topic) => combinedClauses exists { _.value == t.value }
            } 
            dependencies = dependencies filterNot { _.value == value } 
            dependencies = Dependency(value, merge(filteredTopics ++ combinedClauses) map { 
              _.asInstanceOf[Topic] 
            }) :: dependencies
          } 
          case None => dependencies = Dependency(value, clauses) :: dependencies
        } 
      }
    }

    topics ++ actions ++ conditions ++ dependencies
    
  }

  /**
    * @method toGexf
    * @param term
   **/
  def toGexf(terms : List[Term]) : Gexf = {

    val gexf : Gexf = new GexfImpl();

    gexf.setVisualization(true)

    val graph : Graph = gexf.getGraph()
    graph.setDefaultEdgeType(EdgeType.UNDIRECTED).setMode(Mode.STATIC)

    val attrList : AttributeList = new AttributeListImpl(AttributeClass.NODE)
    graph.getAttributeLists().add(attrList)

    val attType : Attribute = attrList.createAttribute("type", AttributeType.STRING, "type")
  
    def makeNode(typename : String, value : String, terms : List[Term]) : Node  = { 

      val node : Node = graph.createNode(value).setLabel(value) // Look for or get if existing 
      node.getAttributeValues().addValue(attType, typename)

      for(term <- terms) node.connectTo(value, toNode(term))

      node

    } 

    def toNode(term : Term) : Node = { 
      term match { 
        case Topic(value, abilities) => makeNode("Topic", value, abilities)
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
 
