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
  private[this] var gateStack : Stack[Edge] = Stack.empty[Edge]

  def apply(tree : LinguisticTree) = compileTopics(tree)

  private[this] def compileGates(tree : LinguisticTree,
    sourceOption : Option[Node], stack : Stack[Edge] = Stack.empty[Edge]) : Stack[Edge] = {

    val children = tree.getChildren.asScala

    tree.getLabel match {
      case "PP" | "SBAR" if children.size == 2 => {

        val (left, right) = (children.head, children.last)

        val label = left.terminalValue
        val targets = compileTopics(right)

        var newStack = Stack.empty[Edge]

        for(target <- targets) {

          val source = sourceOption.getOrElse(target)

          val edge = model.factory.newEdge(source, target)
          edge.setLabel(label)

          model.getGraph.addEdge(edge)

          newStack = newStack.push(edge)
        }

        newStack ++ stack


      }
    }

  }

  private[this] def compileArrows(tree : LinguisticTree,
    stack : Stack[Edge] = Stack.empty[Edge]) : Stack[Edge] = {

    def doubleRules = {
      Set(("VBZ", "VP"), ("VB", "VP"),
        ("VBD", "VP"), ("VBP", "VP"),
        ("VBG", "VP"), ("VBN", "VP"),
        ("TO", "VP"), ("MD", "VP"))
    }

    def predicateRules = {
      Set(("VB", "S"), ("VB", "NP"), ("VB", "@NP"),
        ("VBD", "S"), ("VBD", "NP"), ("VBD", "@NP"),
        ("VBZ", "S"), ("VBZ", "NP"), ("VBZ", "@NP"),
        ("VBP", "S"), ("VBP", "NP"), ("VBP", "@NP"),
        ("VBG", "S"), ("VBG", "NP"), ("VBG", "@NP"),
        ("VBN", "S"), ("VBN", "NP"), ("VBN", "@NP"))
    }

    def gateRules = {
      Set(("VB", "PP"), ("VB", "SBAR"),
        ("VBD", "PP"), ("VBD", "SBAR"),
        ("VBZ", "PP"), ("VBZ", "SBAR"),
        ("VBP", "PP"), ("VBP", "SBAR"),
        ("VBG", "PP"), ("VBG", "SBAR"),
        ("VBN", "PP"), ("VBN", "SBAR"))
    }

    tree.getLabel match {
      case "VP" | "PP" if tree.existsBinaryRules(predicateRules) => {

        val (left, right) = tree.findBinaryRules(predicateRules).get

        var newStack = Stack.empty[Edge]
        val lastOption = topicStack.lastOption
        val targets = compileTopics(right)

        for {
          source <- lastOption
          target <- targets.headOption
        } model.getGraph.addEdge {

          val edge = model.factory.newEdge(source, target)
          edge.setLabel(left.terminalValue)

          newStack.push(edge)

          edge

        }

        newStack ++ stack

      }
      case "VP" | "PP" if tree.existsBinaryRules(gateRules) => {

        val (left, right) = tree.findBinaryRules(gateRules).get

        var newStack = Stack.empty[Edge]

        val headOption = topicStack.headOption

        val label = left.terminalValue

        var targetGates = gateStack

        gateStack = compileGates(right, headOption)

        targetGates ++= gateStack

        for(source <- headOption) {
          model.getGraph.addEdge {

            val newEdge = model.factory.newEdge(source, source)
            newEdge.setLabel(label)

            newStack = newStack.push(newEdge)

            newEdge
          }
        }

        for {
          source <- headOption
          targetGate <- targetGates
        } model.getGraph.addEdge {

          val target = targetGate.getSource

          val newEdge = model.factory.newEdge(source, target)

          Option(targetGate.getLabel) match {
            case Some(label) => newEdge.setLabel(label)
            case None => Unit
          }

          newStack = newStack.push(newEdge)

          newEdge

        }

        newStack ++ stack

      }
      case "VP" if tree.existsBinaryRules(doubleRules) => {

        val (_, right) = tree.findBinaryRules(doubleRules).get

        stack ++ compileArrows(right)

      }
      case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => {

        var newStack = Stack.empty[Edge]

        val label = tree.terminalValue

        for {
          source <- topicStack.headOption
        } model.getGraph.addEdge {

          val newEdge = model.factory.newEdge(source, source)
          newEdge.setLabel(label)

          newStack = newStack.push(newEdge)

          newEdge

        }

        newStack ++ stack

      }
      case "NP" | "@NP" => {

        compileTopics(tree)

        stack

      }
      case _ => {

       tree.getChildren.asScala.foldLeft(stack) {
          (s : Stack[Edge], c : LinguisticTree) => {
            compileArrows(c,s)
          }
        }

      }
    }

  }

  private[this] def compileTopics(tree : LinguisticTree) : Stack[Node] = {

    def thatRules  = {
      Set(("NP", "SBAR"), ("NP", "PP"), ("@NP", "SBAR"), ("@NP", "PP"))
    }

    def propRules = {
      Set(("IN", "NP"), ("IN", "@NP"), ("IN", "VP"), ("IN", "S"))
    }

    def gerundRules = {
      Set(("VBG", "NP"), ("VBG", "@NP"))
    }

    tree.getLabel match {
      case "NP" | "@NP" if !tree.existsBinaryRules(thatRules) => {

        val topic = Topic(tree.terminalValue)

        for(gate <- gateStack.headOption) {
          model.getGraph.addEdge {

            val edge = model.factory.newEdge(gate.getSource, topic)

            Option(edge.getLabel) match {
              case Some(label) => edge.setLabel(label)
              case None => Unit
            }

            edge
          }
        }

        topicStack = topicStack.push(topic)

        topicStack

      }
      case "VP" if tree.existsBinaryRules(gerundRules) => {

        val (left, right) = tree.findBinaryRules(gerundRules).get

        topicStack = topicStack.push(Topic(right.terminalValue))

        compileArrows(left) // Makes topic connect to itself

        topicStack

      }
      case "VP" => {

        compileArrows(tree)

        topicStack

      }
      case "NP" | "@NP" if tree.existsBinaryRules(thatRules) => {

        val (left, right) = tree.findBinaryRules(thatRules).get

        val headOption = topicStack.headOption
        val newTopics = compileTopics(left)

        gateStack = gateStack ++ compileGates(right, headOption)

        newTopics ++ topicStack

      }
      case "SBAR" | "PP" if tree.existsBinaryRules(propRules) => {

        val headOption = topicStack.headOption

        gateStack = gateStack ++ compileGates(tree, headOption)

        topicStack

      }
      case _ => {

        tree.getChildren.asScala.foldLeft(topicStack) {
          (s : Stack[Node], c : LinguisticTree) => compileTopics(c)
        }

      }
    }

  }
}

object Topic {

  def apply(label : String)(implicit model : GraphModel) : Node = {

    val topic = Option(model.getGraph.getNode(label)) match {
      case Some(topic) => topic
      case None => {

        val topic = model.factory.newNode(label)

        topic.setLabel(label)

        model.getGraph.addNode(topic)

        topic
      }
    }

    topic

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

  import scala.collection.mutable.HashMap


  def apply(model : GraphModel) : Gexf = {

    val gexf : Gexf = new GexfImpl()

    gexf.setVisualization(true)

    val graph : Graph = gexf.getGraph()
    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    val nodeTable = HashMap.empty[Object, Node]

    val nodes = model.getGraph.getNodes.asScala
    val edges = model.getGraph.getEdges.asScala

    for(node <- nodes) {

      val gexfNode = graph.createNode(node.getId.toString).setLabel(node.getLabel)

      nodeTable += node.getId -> gexfNode

    }

    for(edge <- edges) {

      val sourceGexfNode = nodeTable.get(edge.getSource.getId)
      val targetGexfNode = nodeTable.get(edge.getTarget.getId)

      (sourceGexfNode, targetGexfNode) match {
        case (Some(source), Some(target)) => {

          val gexfEdge = source.connectTo(target)
          gexfEdge.setEdgeType(EdgeType.DIRECTED)

          Option(edge.getLabel) match {
            case Some(label) => gexfEdge.setLabel(label)
            case None => Unit
          }

        }
        case _ => Unit
      }

    }

    gexf

  }

}

