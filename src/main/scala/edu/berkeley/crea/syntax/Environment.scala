package edu.berkeley.crea.syntax

import java.util.Calendar
import java.io.StringWriter

import scala.util.Random

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.{ 
  List, Set, Map, Stack
}

import it.uniroma1.dis.wsngroup.gexf4j.core.{ 
  EdgeType, Gexf, Graph, Mode, Node, Edge
}

import it.uniroma1.dis.wsngroup.gexf4j.core.data.{ 
  Attribute, AttributeClass, AttributeList, AttributeType
}

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.{ 
  GexfImpl, StaxGraphWriter
}

import it.uniroma1.dis.wsngroup.gexf4j.core.viz.{
  NodeShape, EdgeShape, Color
}

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.data.AttributeListImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.viz.ColorImpl

import TreeConversions._

/** 
  * @class Environment
  * An environment stores and updates 
  * information as sentences are inserted 
  * into it. 
 **/
class Environment {

  import Environment.{ 
    Term, Topic, Condition, Action, Dependency
  } 

  private var topicMap : Map[String, Topic] = Map.empty
  private var currentSize : Int = 0 // Number of unique terms inserted so far.

  /**
    * @method size
    * @return - Number of unique terms in the environment.
   **/
  def size() : Int = currentSize

  private def insert(terms : List[Term]) : Unit = for(term <- terms) {

    term match { 
      case topic : Topic => {

        topicMap.get(topic.value) match { 
          case Some(existingTopic) => {

            val newTerms : List[Term] = { 
              mergeTerms(existingTopic.abilities ++ topic.abilities)
            } 

            val newTopic : Topic = { 
              existingTopic.copy(abilities = newTerms) 
            } 

            topicMap += existingTopic.value -> newTopic

          } 
          case None => { 
            currentSize += 1
            topicMap += topic.value -> topic
          } 
        }
        
        insert(topic.abilities) 

      } 
      case action : Action => { 
        currentSize += 1 
        insert(action.dependencies) 
      } 
      case condition : Condition => { 
        currentSize += 1
        insert(condition.actions) 
      } 
      case dependency : Dependency => { 
        currentSize += 1
        insert(dependency.topics) 
      } 

    }
  } 


  /** 
    * @method insertTopics
    * @param topics - The list of topics to be loaded 
    * into the environment. 
   **/
  def insertTopics(topics : List[Topic]) : Unit = insert(topics)

  /** 
    * @method selectTopics
    * @return - All topics found in the environment, where 
    * topics inserted with the same values have been merged together. 
    **/
  def selectTopics() : List[Topic] = {

    var selectedTopics : Map[String, Topic] = Map.empty[String, Topic]
    var selecting : Stack[Topic] = Stack.empty[Topic]

    def isSelecting(t : Topic) : Boolean = selecting exists { 
      _.value == t.value
    } 

    def select(term : Term) : Term = term match { 
      case topic : Topic => {

        selecting = selecting.push(topic) 

        val newTopic : Topic = selectedTopics.get(topic.value) match { 
          case Some(selectedTopic) => selectedTopic
          case None => { 

            val newTopic : Topic = { 
              val oldTopic : Topic = topicMap.get(topic.value).get
              oldTopic.copy(abilities = oldTopic.abilities.map(select)) 
            }

            selectedTopics += topic.value -> newTopic

            newTopic

          }

        }

        selecting = selecting.pop

        newTopic 

      }
      case action : Action => { 
        action.copy(dependencies = action.dependencies.map(select)) 
      } 
      case condition : Condition => { 
        condition.copy(actions = condition.actions.map(select))
      }
      case dependency : Dependency => {

        val newTopics : List[Term] = { 
          dependency.topics.collect { case t : Topic => t}.filterNot(isSelecting).map(select)
        }

        dependency.copy(topics = newTopics)

      } 
    } 

    val topics = topicMap.values.map(select).collect{ case t : Topic => t }.toList 

    topics

  }

  /** 
    *  As seen on StackOverflow:
    *  http://stackoverflow.com/questions/3912753/scala-remove-duplicates-in-list-of-objects 
   **/
  private def uniqueValues(lst : List[Term]) : List[Term] = { 

    lst filterNot { 

      var set : Set[String] = Set.empty[String]

      clause => { 
        val b : Boolean = set(clause.value)
        set += clause.value
        b
      }

    } 

  } 

  /**
    * @method mergeTerms
    * @param terms {List[Term]}
    * @return {List[Term]}
   **/
  private def mergeTerms(terms : List[Term]) : List[Term] = {

    var map : Map[String, Term] = Map.empty[String, Term]

    for(term <- terms) { 
      map.get(term.value) match { 
        case Some(t) => map += term.value -> { 
          term match {
            case topic : Topic if t.isInstanceOf[Topic] => { 
              topic.copy(abilities = { 
                mergeTerms(topic.abilities ++ t.asInstanceOf[Topic].abilities)
              })
            }
            case condition : Condition if t.isInstanceOf[Condition] => { 
              condition.copy(actions = { 
                mergeTerms(condition.actions ++ t.asInstanceOf[Condition].actions)
              }) 
            } 
            case action : Action if t.isInstanceOf[Action] => { 
              action.copy(dependencies = { 
                mergeTerms(action.dependencies ++ t.asInstanceOf[Action].dependencies)
              })
            } 
            case dependency : Dependency if t.isInstanceOf[Dependency] => { 
              dependency.copy(topics = { 
                uniqueValues(dependency.topics ++ t.asInstanceOf[Dependency].topics) 
              }) 
            }
          } 
        } 
        case None => map += term.value -> term
      } 
    }

    map.values.toList

  } 
}

/** 
  * @object Environment
 **/ 
object Environment {

  val defaultColor : Color = new ColorImpl(0, 0, 0) 

  sealed abstract class Term {
    def value : String;
    def color : Color = defaultColor
  }

  case class Action(value : String, dependencies : List[Term]) extends Term { 

    override def equals(other : Any) : Boolean = other match { 
      case action : Action => { 

        action.value == this.value && { 

          action.dependencies.zip(this.dependencies).map { 
            t => t._1 == t._2
          }.foldRight(true) (_ && _) 

        } 

      } 
      case _ => false 
    } 

  } 

  case class Dependency(value : String, topics : List[Term]) extends Term {

    override def equals(other : Any) : Boolean = other match { 
      case dependency : Dependency  => { 

        dependency.value == this.value && { 

          dependency.topics.zip(this.topics).map { 
            t => t._1 == t._2
          }.foldRight(true) (_ && _) 

        } 

      } 
      case _ => false 
    } 

  } 

  case class Condition(value : String, actions : List[Term]) extends Term { 

    override def equals(other : Any) : Boolean = other match { 
      case condition : Condition => { 

        condition.value == this.value && { 

          condition.actions.zip(this.actions).map { 
            t => t._1 == t._2
          }.foldRight(true) (_ && _) 

        } 

      } 
      case _ => false 
    } 

  } 

  case class Topic(value : String, abilities : List[Term]) extends Term { 

    override def equals(other : Any) : Boolean = other match { 
      case topic : Topic  => { 

        topic.value == this.value && { 

          topic.abilities.zip(this.abilities).map { 
            t => t._1 == t._2
          }.foldRight(true) (_ && _) 

        } 

      } 
      case _ => false 
    } 

  } 


  def parse(tree : LinguisticTree) : List[Topic] = { 
    val parser : EnvironmentParser = new EnvironmentParser
    parser.parse(tree).toList
  } 

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

    // Map of already visited topic nodes
    var visitedTopics : Map[Topic, Node] = Map.empty[Topic, Node] 

    // Map of already visited dependencies, to make two different actions
    // point to the same dependency. 
    var visitedDependencies : Map[Dependency, Node] = Map.empty[Dependency, Node]

    object IDs { 
      private var currentNodeId : Int = 0 
      private var currentEdgeId : Int = 0 

      def nextNode() : String = { 
        val id : Int = currentNodeId
        currentNodeId += 1
        id.toString
      }

      def nextEdge() : String = { 
        val id : Int = currentEdgeId
        currentEdgeId += 1
        id.toString
      } 

    } 

    def makeEdges(node : Node, terms : List[Term]) : List[Edge] = { 

      var edges : List[Edge] = List.empty[Edge]
      
      for(term <- terms) {

        val newEdges : List[Edge] = term match { 
          case dep : Dependency if dep.value == "" => { 
            makeEdges(node, dep.topics) 
          } 
          case _ => try {

            val edge : Edge = node.connectTo(IDs.nextEdge, toNode(term))

            edge.setEdgeType(EdgeType.DIRECTED)
            edge.setColor(new ColorImpl(0,0,0))

            term match { 
              case _ : Dependency => edge.setShape(EdgeShape.DASHED)
              case _ : Action => edge.setShape(EdgeShape.DASHED)
              case _ => Unit
            } 

            List[Edge](edge) 
          } catch { 
            case e : IllegalArgumentException => List.empty[Edge]
          } 
        } 

        edges = edges ++ newEdges 

      }

      edges
    } 

    def makeNode(typename : String)(value : String, terms : List[Term]) : Node = {

      val node : Node = graph.createNode(IDs.nextNode).setLabel(value)  

      node.getAttributeValues().addValue(attType, typename)

      makeEdges(node, terms)

      node

    } 

    def toNode(term : Term) : Node = {

      term match { 
        case topic : Topic => visitedTopics.get(topic).getOrElse({ 
          val node : Node = makeNode("Topic")(topic.value, topic.abilities)
          visitedTopics += topic -> node
          node
        })
        case dep : Dependency => {
         
          visitedDependencies.get(dep) match {
            case Some(node) => node
            case None => { 
              var node : Node = makeNode("Dependency")(dep.value, dep.topics)
              visitedDependencies += dep -> node
              node 
            }
          }

        } 
        case condition : Condition => { 
          val node : Node = makeNode("Condition")(condition.value, condition.actions)
          node.getShapeEntity.setNodeShape(NodeShape.SQUARE) 
          node 
        } 
        case a : Action => {
          makeNode("Action")(a.value, a.dependencies) 
        } 
      }

    }
    
    for (term <- terms) yield toNode(term) 
    
    gexf

  }


} 
 
