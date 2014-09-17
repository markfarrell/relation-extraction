package crea.nlp

import java.io.{ByteArrayInputStream, OutputStream, InputStream, File, FileInputStream, FileOutputStream, PrintStream, PrintWriter}

import edu.berkeley.nlp.syntax.Trees.PennTreeReader

import scala.io.Source

import scalaz._
import Scalaz._

import scalaz.concurrent._
import scalaz.stream._

import Terms._
import Trees._

class Document(src : InputStream) {

  private[this] lazy val parser = new Parser
  private[this] val bufSize = Runtime.getRuntime.availableProcessors

  private[this] def parse(token : String) : Tree[String] = parser(token)

  lazy val tokens : Process[Task, String] = {

    def removeParens = (_ : String).replaceAll("""\s{0,1}\(.*?\)""", "")

    Process.emitSeq(Tokenize(src).map(removeParens).toSeq)

  }

  lazy val compiles : Process[Task, Tree[String] \/ List[Relation]] = tokens.gatherMap(bufSize)(token => Task.delay(Compile(parse(token))))

  lazy val log : Task[Unit] = compiles.gatherMap(bufSize)(res => Task.delay(res.shows)).to(io.stdOutLines).run

}

object Document {

  private[this] def channel[A,B](f : A => Task[B]) : Channel[Task, A, B] = Process.constant(f)

  def fromText(text : String) : Document = new Document(new ByteArrayInputStream(text.getBytes))

  def fromFile(path : String) : Document = new Document(new FileInputStream(path))

  def stdGexf : Sink[Task, Tree[String] \/ List[Relation]] = gexf(System.out)

  def gexf(dest : OutputStream) : Sink[Task, Tree[String] \/ List[Relation]] = {

    import it.uniroma1.dis.wsngroup.gexf4j.core.impl.{StaxGraphWriter, GexfImpl}
    import it.uniroma1.dis.wsngroup.gexf4j.core.{EdgeType, Mode, Node}
    import scala.collection.mutable.HashMap

    val charset = "UTF-8"

    val gexf = new GexfImpl
    val graph = gexf.getGraph

    val writer = new StaxGraphWriter

    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    val nodeTable = HashMap.empty[String, Node]

    channel { res => Task.delay { res.foreach { relations =>

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

        \/.fromTryCatch(sourceNode.connectTo(targetNode).setLabel(relation.literal.id))

      }

      writer.writeToStream(gexf, dest, charset)

    }}}

  }

  def stdCypher : Sink[Task, Tree[String] \/ List[Relation]] = cypher(System.out)

  def cypher(out : OutputStream) : Sink[Task, Tree[String] \/ List[Relation]] = {

    val pw = new PrintWriter(out)

    channel { res => Task.delay { res.foreach { relations =>

      for {

        relation <- relations
        (source, target) <- relation.args.sliding(2).map(s => (s.head, s.last))

      } {

        val pattern = """\s|\p{Punct}"""
        val sourceId = source.id.replaceAll(pattern, "_")
        val targetId = target.id.replaceAll(pattern, "_")
        val edgeId = relation.literal.id.replaceAll("""\s""", "_").toUpperCase

        pw.println(s"""MERGE (${sourceId}:Literal {label: "${source.id}"})""")

        if(!(source.id === target.id)) {

          pw.println(s"""MERGE (${targetId}:Literal {label: "${target.id}"})""")

        }

        pw.println(s"""CREATE UNIQUE (${sourceId})-[:${edgeId}]->(${targetId});""")
        pw.println()
        pw.flush()

      }

    }}}

  }

}
