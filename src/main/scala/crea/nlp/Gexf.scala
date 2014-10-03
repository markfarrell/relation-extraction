package crea.nlp

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.{StaxGraphWriter, GexfImpl}
import it.uniroma1.dis.wsngroup.gexf4j.core.{EdgeType, Mode, Node}
import edu.cmu.lti.lexical_db.NictWordNet
import edu.cmu.lti.ws4j.impl.WuPalmer
import scala.collection.mutable.HashMap
import java.io.ByteArrayOutputStream

import scalaz._
import Scalaz._
import Terms._

object Gexf { 

  def apply(relations : List[Relation]) : String = {

    val charset = "UTF-8"

    val gexf = new GexfImpl
    val graph = gexf.getGraph

    val writer = new StaxGraphWriter

    val db = new NictWordNet
    val rc = new WuPalmer(db)

    val dest = new ByteArrayOutputStream

    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    val nodeTable = HashMap.empty[String, Node]

    for {

      relation <- relations
      (source, target) <- relation.args.sliding(2).map(s => (s.head, s.last))

    } {

      val sourceNode = nodeTable.get(source.id).getOrElse {
        nodeTable(source.id) = graph.createNode(source.id).setLabel(source.id)
        nodeTable(source.id)
      }

      val targetNode = nodeTable.get(target.id).getOrElse {
        nodeTable(target.id) = graph.createNode(target.id).setLabel(target.id)
        nodeTable(target.id)
      }

      \/.fromTryCatch {

        val label = relation.literal.id
        val edge = sourceNode.connectTo(targetNode)

        edge.setLabel(label)
        edge.setWeight(rc.calcRelatednessOfWords(s"${label}#v","increase#v").toFloat)

        edge

      }

    }

    writer.writeToStream(gexf, dest, charset)

    dest.toString()

  }


} 
