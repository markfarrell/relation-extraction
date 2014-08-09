package edu.crea.nlp

import java.io.{InputStream, File, FileInputStream, FileOutputStream, PrintStream}

import edu.berkeley.nlp.syntax.Trees.PennTreeReader

import scala.io.Source

import scalaz._
import Scalaz._

import scalaz.concurrent._
import scalaz.stream._

import Patterns._
import Terms._
import Trees._

class Document(src : InputStream) {

  private[this] lazy val parser = new Parser
  private[this] val bufSize = math.max(20, Runtime.getRuntime.availableProcessors)

  private[this] def parse(token : String) : Tree[String] = parser(token)

  private[this] def compile(tree : Tree[String]) : Tree[String] \/ Stream[Compound] = RootExpression(tree) match {
    case Some(compounds) => compounds.right
    case None => tree.left
  }

  lazy val tokens : Process[Task, String] = {

    def removeParens = (_ : String).replaceAll("""\s{0,1}\(.*?\)""", "")

    io.resource(Task.delay(src))(src => Task.delay(src.close)) { src =>
      val it = Tokenize(src).map(removeParens).iterator
      Task.delay(if (it.hasNext) it.next else throw Process.End)
    }

  }

  lazy val compiles : Process[Task, Tree[String] \/ Stream[Compound]] = tokens.gatherMap(bufSize)(tree => Task.delay(compile(parse(tree))))

  lazy val log : Process[Task, Unit] = compiles.gatherMap(bufSize)(res => Task.delay(res.shows)).to(io.stdOutLines)

}

object Document {

  private[this] def channel[A,B](f : A => Task[B]) : Channel[Task, A, B] = Process.constant(f)

  def fromFile(path : String) : Document = new Document(new FileInputStream(path))

  def gexf : Sink[Task, Tree[String] \/ Stream[Compound]] = {

    import it.uniroma1.dis.wsngroup.gexf4j.core.impl.GexfImpl
    import it.uniroma1.dis.wsngroup.gexf4j.core.{EdgeType, Mode, Node}
    import scala.collection.mutable.HashMap

    val gexf = new GexfImpl
    val graph = gexf.getGraph

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

        val edge = \/.fromTryCatch(sourceNode.connectTo(targetNode).setLabel(compound.atom.id))

        println(s"(${source.id}) |--(${compound.atom.id})--> (${target.id})")

      }

    }}}


  }

}
