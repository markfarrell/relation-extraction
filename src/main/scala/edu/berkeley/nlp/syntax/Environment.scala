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
  case class Action(value : String, dependency : Option[Dependency]) extends Term // VB, VBZ, VBP, VBD, CC
  case class Dependency(value : String, clause : Option[Topic]) extends Term
  case class Condition(modal : String, actionResult: Option[Action]) extends Term
  case class Topic(value : String, ability : Option[Term]) extends Term //VGD, NP, CC 
  //TODO: Add support for multiple actions: e.g. The dog can walk and might run.
  // Possible expand a sentence before passing it to the environment.

  /** 
    * @method toTopic
    * @param tree
    **/
  def toTopic(tree : LinguisticTree) : Option[Topic] = tree.findCut(topicTags) map { 
    (t : LinguisticTree) => Topic(t.terminalList().mkString(" "), toCondition(tree).orElse(toAction(tree)))
  } 

  /**
    * @method toDependency
    * @param tree
   **/
  def toDependency(tree : LinguisticTree) : Option[Dependency] = tree.findCut(dependencyTags) map { 
    (t : LinguisticTree) => Dependency(t.terminalList().mkString(" "), toTopic(tree))
  }

  /**
    * @method toAction
    * @param tree
   **/
  def toAction(tree : LinguisticTree) : Option[Action] = for { 
    t1 <- tree.findCut(vpTags)
    t2 <- t1.findCut(actionTags) 
    action <- Some(Action(t2.terminalList().mkString(" "), toDependency(t1)))
  } yield action


  /**
    * @method toCondition
    * @param tree
   **/
  def toCondition(tree : LinguisticTree) : Option[Condition] = for { 
    t1 <- tree.findCut(vpTags)
    t2 <- t1.findCut(conditionTags) 
    condition <- Some(Condition(t2.terminalList().mkString(" "), toAction(t1)))
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

  def toGexf(term : Term) : Gexf = {

    val gexf : Gexf = new GexfImpl();

    gexf.setVisualization(true)

    val graph : Graph = gexf.getGraph()
    graph.setDefaultEdgeType(EdgeType.UNDIRECTED).setMode(Mode.STATIC)

    val attrList : AttributeList = new AttributeListImpl(AttributeClass.NODE)
    graph.getAttributeLists().add(attrList)

    val attType : Attribute = attrList.createAttribute("type", AttributeType.STRING, "type")
  
    def makeNode(typename : String, value : String, term : Option[Term]) : Node  = { 

      val node : Node = graph.createNode(value).setLabel(value)
      node.getAttributeValues().addValue(attType, typename)

      for { 
        next <- term
      } yield  node.connectTo(value, toNode(next))

      node

    } 

    def toNode(term : Term) : Node = { 
      term match { 
        case Topic(value, ability) => makeNode("Topic", value, ability)
        case Dependency(value, clause) => makeNode("Dependency", value, clause)
        case Condition(modal, action) => makeNode("Condition", modal, action)
        case Action(value, dependency) => makeNode("Action", value, dependency)
      } 
    }

    toNode(term)
    
    gexf

  } 

} 
 
