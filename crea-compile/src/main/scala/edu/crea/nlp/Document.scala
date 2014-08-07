package edu.crea.nlp

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import edu.berkeley.nlp.syntax.Trees.PennTreeReader

import scala.io.Source

import scalaz._
import Scalaz._

import scalaz.concurrent._
import scalaz.stream._

import Patterns._
import Terms._
import Trees._

class Document(path : String) {

  private[this] lazy val parser = new Parser
  private[this] lazy val cores = Runtime.getRuntime.availableProcessors

  private[this] def parse(token : String) : Tree[String] = parser(token)

  private[this] def compile(tree : Tree[String]) : Tree[String] \/ Stream[Compound] = RootExpression(tree) match {
    case Some(compounds) => compounds.right
    case None => tree.left
  }


  lazy val tokens : Process[Task, String] = {

    def src = new FileInputStream(path)
    def removeParens = (_ : String).replaceAll("""\s{0,1}\(.*?\)""", "")

    io.resource(Task.delay(src))(src => Task.delay(src.close)) { src =>
      val it = Tokenize(src).map(removeParens).iterator
      Task.delay(if (it.hasNext) it.next else throw Process.End)
    }

  }

  lazy val parses : Process[Task, Tree[String]] = tokens.gatherMap(cores)(token => Task.delay(parse(token)))

  lazy val compiles : Process[Task, Tree[String] \/ Stream[Compound]] = parses.gatherMap(cores)(tree => Task.delay(compile(tree)))

  lazy val log : Process[Task, Unit] = compiles.map(_.shows).to(io.stdOutLines)

}

object Document {

  private[this] def channel[A,B](f : A => Task[B]) : Channel[Task, A, B] = Process.constant(f)

  def gexf : Sink[Task, Tree[String] \/ Stream[Compound]] = {

    import it.uniroma1.dis.wsngroup.gexf4j.core.impl.GexfImpl
    import it.uniroma1.dis.wsngroup.gexf4j.core.{EdgeType, Mode}

    val gexf = new GexfImpl
    val graph = gexf.getGraph

    graph.setDefaultEdgeType(EdgeType.DIRECTED).setMode(Mode.STATIC)

    channel { res => Task.delay { res.foreach { compounds =>

      for {

        compound <- compounds
        (source, target) <- compound.args.sliding(2).map(s => (s.head, s.last))

      } {

        val edge = graph.createNode(source.id)
          .connectTo(graph.createNode(target.id))
          .setLabel(compound.atom.id)

        println(edge)

      }

    }}}


  }

}
