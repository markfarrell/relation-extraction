package edu.berkeley.crea.syntax

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.{ 
  List, Set, Stack
}

import org.gephi.graph.api.{ Graph, Node, Edge, GraphFactory, GraphModel } 

import TreeConversions._

/** 
 * A Compiler front-end that writes to an intermediate in-memory graphstore, 
 * whose contents can then be written to a GEXF file or database back-end. 
 **/ 
class Compiler(implicit model : GraphModel) {

  private[this] var topicStack : Stack[Node] = Stack.empty[Node]
  private[this] var verbStack : Stack[Node] = Stack.empty[Node] 
  private[this] var dependencyStack : Stack[Node] = Stack.empty[Node]

  /**
    * @method parseDependencies
    * @param tree {LinguisticTree} 
    * @return {Stack[Node]}
   **/
  private[this] def parseDependencies(tree : LinguisticTree, 
    stack : Stack[Node] = Stack.empty[Node]) : Stack[Node] = {
  
    val excludeTopics : Stack[Node] = topicStack

    val children = tree.getChildren.asScala

    tree.getLabel match { 
      case "NP" | "@NP" | "S" => {

        stack.push(Dependency("", topicStack, parse(tree)))

      }
      case "PP" | "SBAR" if children.size == 2 => { 

        val (left, right) = (children.head, children.last)

        stack.push(Dependency(left.terminalValue, topicStack, parse(right)))

      }
    } 

  } 

  /** 
    * @method parseVerb
    * @param tree {LinguisticTree}
    * @return {Stack[Node]}
   **/
  private[this] def parseVerb(tree : LinguisticTree,  
    stack : Stack[Node] = Stack.empty[Node]) : Stack[Node] = {

    def doubleRules = { 
      Set(("VBZ", "VP"), ("VB", "VP"), 
        ("VBD", "VP"), ("VBP", "VP"),
        ("VBG", "VP"), ("VBN", "VP"),
        ("TO", "VP"))
    } 

    def conditionRules  = { 
      Set(("MD", "VP"))
    }

    def dependencyRules = { 
      Set(("VB", "PP"), ("VB", "S"), ("VB", "SBAR"), ("VB", "NP"),
        ("VBD", "PP"), ("VBD", "S"), ("VBD", "SBAR"), ("VBD", "NP"), 
        ("VBZ", "PP"), ("VBZ", "S"), ("VBZ", "SBAR"), ("VBZ", "NP"), 
        ("VBP", "PP"), ("VBP", "S"), ("VBP", "SBAR"), ("VBP", "NP"), 
        ("VBG", "PP"), ("VBG", "S"), ("VBG", "SBAR"), ("VBG", "NP"), 
        ("VBN", "PP"), ("VBN", "S"), ("VBN", "SBAR"), ("VBN", "NP")) 
    }
    
    tree.getLabel match { 
      case "VP" if tree.existsBinaryRules(conditionRules) => { 

        val (left, right) = tree.findBinaryRules(conditionRules).get

        stack.push(Condition(left.terminalValue, parseVerb(right)))

      } 
      case "VP" | "PP" if tree.existsBinaryRules(dependencyRules) => {

        val (left, right) = tree.findBinaryRules(dependencyRules).get

        val targets = parseDependencies(right) ++ dependencyStack

        dependencyStack = Stack.empty[Node]

        stack.push(Action(left.terminalValue, targets)) 

      }
      case "VP" if tree.existsBinaryRules(doubleRules) => {

        val (_, right) = tree.findBinaryRules(doubleRules).get

        stack ++ parseVerb(right) 

      } 
      case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => { 

        val targets = dependencyStack

        dependencyStack = Stack.empty[Node] 

        stack.push(Action(tree.terminalValue, targets)) 

      }
      case "NP" if stack.size > 0 => { 

        val action = stack.head

        val targets = parseDependencies(tree) ++ dependencyStack
        dependencyStack = Stack.empty[Node]

        for(node <- targets) { 
          model.getGraph.addEdge { 
            model.factory.newEdge(action, node) 
          } 
        } 

        stack

      }
      case _ => { 

       tree.getChildren.asScala.foldLeft(stack) { 
          (s : Stack[Node], c : LinguisticTree) => { 
            parseVerb(c,s)
          } 
        } 

      } 
    } 
    
  } 


  def parse(tree : LinguisticTree) : Stack[Node] = {

    def thatRules  = { 
      Set(("NP", "SBAR"), ("NP", "PP"), ("@NP", "SBAR"), ("@NP", "PP"))
    }

    def propRules = { 
      Set(("IN", "NP"), ("IN", "@NP"), ("IN", "VP"), ("IN", "S"))
    } 

    def gerundRules = { 
      Set(("VBG", "NP"))
    } 

    tree.getLabel match {
      case "NP" | "@NP" if !tree.existsBinaryRules(thatRules) => {

        val targets = verbStack

        verbStack = Stack.empty[Node] 

        topicStack = topicStack.push(Topic(tree.terminalValue, targets))

        topicStack

      }
      case "VP" if tree.existsBinaryRules(gerundRules) => { 
        
        val (left, right) = tree.findBinaryRules(gerundRules).get

        val targets = verbStack ++ parseVerb(left)
        verbStack = Stack.empty[Node]

        topicStack = topicStack.push(Topic(right.terminalValue, targets))

        topicStack 

      } 
      case "VP" if topicStack.size > 0 => { 

        val topic = topicStack.head
        topicStack = topicStack.pop

        val targets = parseVerb(tree) ++ verbStack 
        verbStack = Stack.empty[Node] 

        for(node <- targets) { 
          model.getGraph.addEdge { 
            model.factory.newEdge(topic, node) 
          } 
        } 

        topicStack.push(topic)

      }
      case "VP" => { 

        verbStack = verbStack ++ parseVerb(tree)

        topicStack

      } 
      case "NP" | "@NP" if tree.existsBinaryRules(thatRules) => { 

        val (left, right) = tree.findBinaryRules(thatRules).get

        dependencyStack = { 
          dependencyStack ++ parseDependencies(left) ++ parseDependencies(right)
        }

        topicStack

      } 
      case "SBAR" | "PP" if tree.existsBinaryRules(propRules) => { 

        dependencyStack = dependencyStack ++ parseDependencies(tree) 

        topicStack 

      } 
      case _ => {

        tree.getChildren.asScala.foldLeft(topicStack) { 
          (s : Stack[Node], c : LinguisticTree) => parse(c)
        } 

      } 
    } 

  } 
}

object Topic { 

  def apply(label : String, targets : Stack[Node])(implicit model : GraphModel) : Node = {

    val topic = Option(model.getGraph.getNode(label)) match { 
      case Some(topic) => topic 
      case None => { 

        val topic = model.factory.newNode(label)

        topic.setLabel(label) 
        //topic.setAttribute("type", "Topic") 

        model.getGraph.addNode(topic) 

        topic 
      }
    }

    for(node <- targets) { 
      model.getGraph.addEdge { 
        model.factory.newEdge(topic, node) 
      } 
    } 

    topic

  } 

} 

object Dependency { 

  def apply(label : String, targets : Stack[Node], sources : Stack[Node])(implicit model : GraphModel) : Node = {

    val dependency = model.factory.newNode() 

    dependency.setLabel(label)
    //dependency.setAttribute("type", "Dependency")

    model.getGraph.addNode(dependency)

    for(source <- sources) { 
      model.getGraph.addEdge { 
        model.factory.newEdge(source, dependency)
      } 
    } 

    for(target <- targets) { 
      model.getGraph.addEdge { 
        model.factory.newEdge(dependency, target)
      } 
    } 

    dependency

  }

  private def containsTopic(topics : Stack[Node])(topic : Node) : Boolean = { 
    topics.find(_.getLabel == topic.getLabel) match { 
      case Some(_) => true
      case None => false
    } 
  }

} 

object Action { 

  def apply(label : String, targets : Stack[Node])(implicit model : GraphModel) : Node = { 

    val action = model.factory.newNode() 
    action.setLabel(label) 
    //action.setAttribute("type", "Action") 

    model.getGraph.addNode(action) 

    for(node <- targets) { 
      model.getGraph.addEdge { 
        model.factory.newEdge(action, node) 
      } 
    }

    action

  } 

}

object Condition { 

  def apply(label : String, targets : Stack[Node])(implicit model : GraphModel) : Node = { 

    val condition = model.factory.newNode()
    condition.setLabel(label) 
    //condition.setAttribute("type", "Condition") 

    model.getGraph.addNode(condition)

    for(node <- targets) { 
      model.getGraph.addEdge { 
        model.factory.newEdge(condition, node)
      } 
    } 

    condition 

  }

}

object ToGexf {

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

  def apply(model : GraphModel) : Gexf = {

    val gexf : Gexf = new GexfImpl()

    gexf.setVisualization(true)

    val graph : Graph = gexf.getGraph()
    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    val attrList : AttributeList = new AttributeListImpl(AttributeClass.NODE)
    graph.getAttributeLists().add(attrList)

    //val attType : Attribute = attrList.createAttribute("type", AttributeType.STRING, "type")

    import scala.collection.mutable.HashMap 
    val nodeTable = HashMap.empty[Object, Node] 

    val nodes = model.getGraph.getNodes.asScala 
    val edges = model.getGraph.getEdges.asScala

    for(node <- nodes) {

      val gexfNode = graph.createNode(node.getId.toString).setLabel(node.getLabel)

      //val valueOfType = node.getAttribute("type").toString
      //assert(valueOfType != null) 
      //gexfNode.getAttributeValues.addValue(attType, valueOfType)

      nodeTable += node.getId -> gexfNode

    }

    for(edge <- edges) {

      val sourceGexfNode = nodeTable.get(edge.getSource.getId) 
      val targetGexfNode = nodeTable.get(edge.getTarget.getId)

      (sourceGexfNode, targetGexfNode) match { 
        case (Some(source), Some(target)) => { 

          val gexfEdge = source.connectTo(target) 
          gexfEdge.setEdgeType(EdgeType.DIRECTED) 

        } 
        case _ => Unit 
      } 

    } 

    gexf 

  } 

} 

