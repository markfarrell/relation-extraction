package edu.crea.gexf

import edu.crea.nlp.Terms._

import org.gephi.graph.api.{GraphModel, Node, Edge}

object ToGraph {

  def apply(clauses : Stream[Compound], model : GraphModel) : Unit = {

    for {
      compound <- clauses
      (sourceAtom, targetAtom) <- compound.args.sliding(2).map(stream => (stream.head, stream.last))
    } {

      val label = compound.atom.id
      val source = createNode(sourceAtom.id, model)
      val target = createNode(targetAtom.id, model)

      createEdge(label, source, target, model)

    }

  }

  private[this] def createNode(label : String, model : GraphModel) : Node = {

    Option(model.getGraph.getNode(label)) match {
      case Some(node) => node
      case None => {

        val node = model.factory.newNode(label)

        node.getNodeData.setLabel(label)
        node.getNodeData.setColor(0.5f, 0.5f, 0.5f)

        model.getGraph.addNode(node)

        node
      }
    }

  }

  private[this] def createEdge(label : String, source : Node, target : Node, model : GraphModel) : Edge = {

    val edge = model.factory.newEdge(source, target)

    edge.getEdgeData.setColor(0.5f, 0.5f, 0.5f)
    edge.getEdgeData.setLabel(label)

    model.getGraph.addEdge(edge)

    edge

  }

}
