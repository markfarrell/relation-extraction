package crea.nlp

import java.io.{ByteArrayInputStream, OutputStream, InputStream, File, FileInputStream, FileOutputStream, PrintStream}

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

  lazy val compiles : Process[Task, Tree[String] \/ List[Compound]] = tokens.gatherMap(bufSize)(token => Task.delay(Compile(parse(token))))

  lazy val log : Task[Unit] = compiles.gatherMap(bufSize)(res => Task.delay(res.shows)).to(io.stdOutLines).run

}

object Document {

  private[this] def channel[A,B](f : A => Task[B]) : Channel[Task, A, B] = Process.constant(f)

  def fromText(text : String) : Document = new Document(new ByteArrayInputStream(text.getBytes))

  def fromFile(path : String) : Document = new Document(new FileInputStream(path))

  def stdGexf : Sink[Task, Tree[String] \/ List[Compound]] = gexf(System.out)

  def gexf(dest : OutputStream) : Sink[Task, Tree[String] \/ List[Compound]] = {

    import it.uniroma1.dis.wsngroup.gexf4j.core.impl.{StaxGraphWriter, GexfImpl}
    import it.uniroma1.dis.wsngroup.gexf4j.core.{EdgeType, Mode, Node}
    import scala.collection.mutable.HashMap

    val charset = "UTF-8"

    val gexf = new GexfImpl
    val graph = gexf.getGraph

    val writer = new StaxGraphWriter

    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    val nodeTable = HashMap.empty[String, Node]

    channel { res => Task.delay { res.foreach { compounds =>

      for {

        compound <- compounds
        (source, target) <- compound.args.sliding(2).map(s => (s.head, s.last))

      } {

        val sourceNode = nodeTable.get(source.id).getOrElse {
          nodeTable(source.id) = graph.createNode(source.id).setLabel(source.id)
          nodeTable(source.id)
        }

        val targetNode = nodeTable.get(target.id).getOrElse {
          nodeTable(target.id) = graph.createNode(target.id).setLabel(target.id)
          nodeTable(target.id)
        }

        \/.fromTryCatch(sourceNode.connectTo(targetNode).setLabel(compound.atom.id))

      }

      writer.writeToStream(gexf, dest, charset)

    }}}

  }

}
