package edu.crea.gexf

import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap

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

import org.gephi.graph.api.GraphModel

object ToGexf {

  def apply(model : GraphModel) : Gexf = {

    val gexf : Gexf = new GexfImpl()

    gexf.setVisualization(true)

    val graph : Graph = gexf.getGraph()
    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    val nodeTable = HashMap.empty[Int, Node]

    val nodes = model.getGraph.getNodes.asScala
    val edges = model.getGraph.getEdges.asScala

    for(node <- nodes) {

      val gexfNode = graph.createNode(node.getId.toString).setLabel(node.getNodeData.getLabel)

      nodeTable += node.getId -> gexfNode

    }

    for(edge <- edges) {

      val sourceGexfNode = nodeTable.get(edge.getSource.getId)
      val targetGexfNode = nodeTable.get(edge.getTarget.getId)

      (sourceGexfNode, targetGexfNode) match {
        case (Some(source), Some(target)) => {

          val gexfEdge = source.connectTo(target)
          gexfEdge.setEdgeType(EdgeType.DIRECTED)

          Option(edge.getEdgeData.getLabel) match {
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
