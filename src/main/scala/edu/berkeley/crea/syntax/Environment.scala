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
            existingTopic.value,
            mergeTerms(topic.abilities, existingTopic.abilities), 
            topic.color
          )

          topicMap += existingTopic.value -> updatedTopic

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

    var selectedTopics : Map[String, Topic] = Map.empty[String, Topic]
    var selecting : Stack[Topic] = Stack.empty[Topic]

    def isSelecting(t : Topic) : Boolean = selecting exists { 
      _.value == t.value
    } 

    def selectTopic(t : Topic) : Topic = {

      selecting = selecting.push(t) 

      val newTopic : Topic = selectedTopics.get(t.value) match { 
        case Some(selectedTopic) => selectedTopic
        case None => { 

          val newTopic : Topic = { 

            val topic : Topic = topicMap.get(t.value).get 

            Topic( 
              topic.value,
              selectActions(topic.abilities) ++ selectConditions(topic.abilities),
              topic.color
            )

          } 

          selectedTopics += newTopic.value -> newTopic

          newTopic

        }
        
      }

      selecting = selecting.pop

      newTopic 

    } 

    def selectActions(terms : List[Term]) : List[Action] = for { 
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
      dependency.clauses.filterNot(isSelecting).map(selectTopic),
      dependency.color
    )
    
    val topics : Iterable[Topic] = for { 
      topic <- topicMap.values 
    } yield selectTopic(topic)

    topics.toList

  }

  /** 
    * @method getActions
    * @param terms {List[Term} 
    * @return {List[Action]}
   **/
  private def getActions(terms : List[Term]) : List[Action] = terms collect { 
    case a : Action => a
  } 

  /**
    * @method getConditions
    * @param terms {List[Terms]}
    * @return {List[Condition]}
   **/
  private def getConditions(terms : List[Term]) : List[Condition] = terms collect { 
    case c : Condition => c
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

            // As seen on StackOverflow:
            // http://stackoverflow.com/questions/3912753/scala-remove-duplicates-in-list-of-objects

            val uniqueClauses : List[Topic] = { 
              (d.clauses ++ dependency.clauses) filterNot { 

                var set : Set[String] = Set.empty[String]

                clause => { 
                  val b : Boolean = set(clause.value)
                  set += clause.value
                  b
                } 

              } 
            } 

            map += d.value -> Dependency(d.value, 
              uniqueClauses,
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

/** 
  * @object Environment
 **/ 
object Environment { 

  // Set to false to make nextColor 
  var randomize : Boolean = true 

  private val random : Random = new Random()
  private val defaultColor : Color = new ColorImpl(0,0,0)

  abstract class Term {
    def value : String;
    def color : Color;  
  }

  case class Action(value : String, dependencies : List[Dependency],
    color : Color = defaultColor) extends Term 

  case class Dependency(value : String, clauses: List[Topic],
    color : Color = defaultColor) extends Term { 

    def depEq(d : Dependency) : Boolean = {

      def andmap(t : List[Boolean]) : Boolean = { 
        t.foldRight(true)( _ && _ )
      } 

      val t1 : List[Topic] = clauses
      val t2 : List[Topic] = d.clauses 

      if(t1.size != t2.size) { 
        false
      } else andmap { 

        t1 zip t2 map { 
          case (a,b) => a.value == b.value
        }

      } 

    } 

  } 

  case class Condition(value : String, actions : List[Action],
    color : Color = defaultColor) extends Term

  case class Topic(value : String, abilities : List[Term],
    color : Color = defaultColor) extends Term 


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
    var visitedTopics : Map[String, Node] = Map.empty[String, Node] 

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
            makeEdges(node, dep.clauses) 
          } 
          case _ => try {

            val edge : Edge = node.connectTo(IDs.nextEdge, toNode(term))

            edge.setEdgeType(EdgeType.DIRECTED)
            edge.setColor(term.color)

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
        case Topic(value, abilities, color) => visitedTopics.get(value).getOrElse({ 
          val node : Node = makeNode("Topic")(value, abilities)
          visitedTopics += value -> node
          node
        })
        case dep : Dependency => {
         
          visitedDependencies.find(x => x._1 depEq dep) match {
            case Some(kv) => { 
              val (_, node) = kv
              node
            } 
            case None => { 
              var node : Node = makeNode("Dependency")(dep.value, dep.clauses)
              visitedDependencies += dep -> node
              node 
            }
          }

        } 
        case Condition(value, actions, color) => { 
          val node : Node = makeNode("Condition")(value, actions)
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
 
