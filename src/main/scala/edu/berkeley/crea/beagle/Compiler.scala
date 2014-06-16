package edu.berkeley.crea.beagle

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.{
  List, Set, Stack
}

import edu.berkeley.nlp.syntax.Tree

import java.io.File
import java.io.FileOutputStream

import org.gephi.graph.api.{ Graph, Node, Edge, GraphFactory, GraphModel }

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

import TreeConversions._

/**
  * A one-state pushdown automaton; equivalent to a finite automaton.
  * Accepts linguistic trees as input. Builds nodes and edges on the graph.
  * @param model The [GraphModel] containing a graph.
  * @author Mark Farrell
 **/
class Compiler(model : GraphModel) {

  private[this] var nodeStack : Stack[Node] = Stack.empty[Node]

  def apply(tree : LinguisticTree) : GraphModel = {

    model.getGraph.writeLock()
    compile(tree)
    model.getGraph.writeUnlock()
    reset()
    model

  }

  private[this] def compile(tree : LinguisticTree) : Unit = {

    val children = tree.getChildren.asScala

    tree.getLabel match {
      case "@S" | "S" if tree.existsBinaryRules(propRules) => {

        val (left, right) = tree.findBinaryRules(propRules).get

        compile(right) // Topic
        compile(left) //  If ... then ...

      }
      case "@NP" | "NP" if !tree.existsBinaryRules(thatRules) => {

        for (topic <- Topic(tree.terminalValue)) {
          nodeStack = nodeStack.push(topic)
        }

      }
      case "@VP" | "VP" if tree.existsBinaryRules(gerundRules) => {

        val (left, right) = tree.findBinaryRules(gerundRules).get

        compile(left) // Verb
        compile(right) // Topic

      }
      case "@VP" | "VP" | "PP" if tree.existsBinaryRules(predicateRules) => {

        val (left, right) = tree.findBinaryRules(predicateRules).get

        val sourceOption = nodeStack.headOption

        compile(right) // Verb

        val targetOption = nodeStack.headOption
        val label = left.terminalValue

        for {
          source <- sourceOption
          target <- targetOption
        } Predicate(source, target, label)

      }
      case "@VP" | "VP" | "PP" if tree.existsBinaryRules(nestedRules) => {

        val (left, right) = tree.findBinaryRules(nestedRules).get

        compile(right)
        compile(left)

      }
      case "@VP" | "VP" | "PP" if tree.existsBinaryRules(gateRules) => {

        val (left, right) = tree.findBinaryRules(gateRules).get

        val headOption = nodeStack.headOption
        val label = left.terminalValue

        for(source <- headOption) {
          Predicate(source, source, label)
        }

        compile(right)

      }
      case "@VP" | "VP" if tree.existsBinaryRules(doubleRules) => {

        val (_, right) = tree.findBinaryRules(doubleRules).get

        compile(right)

      }
      case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => {

        val label = tree.terminalValue

        for {
          source <- nodeStack.headOption
        } Predicate(source, source, label)

      }
      case "@NP" | "NP" if tree.existsBinaryRules(thatRules) => {

        val (left, right) = tree.findBinaryRules(thatRules).get

        compile(left)
        compile(right)

      }
      case "@PP" | "PP" | "SBAR" if children.size <= 2 => {

        val sourceOption = nodeStack.headOption

        // Filters the source node: edge loops are not wanted here.
        def ok(target : Node) : Boolean = sourceOption match {
          case Some(source) => source.getNodeData.getId != target.getNodeData.getId
          case None => false
        }

        val subtree = if(children.size == 2) {
          children.last
        } else {
          children.head
        }

        val label = if(children.size == 2) {
          children.head.terminalValue
        } else {
          ""
        }

        compile(subtree)

        val (targets, _) = nodeStack.span(ok)

        for {
          target <- targets.lastOption
          source <- sourceOption
        } Predicate(source, target, label)

      }
      case _ => {

        import org.slf4j.{Logger, LoggerFactory}
        import edu.berkeley.nlp.syntax.Trees.PennTreeRenderer

        val logger = LoggerFactory.getLogger(classOf[Compiler])
        val rendered = PennTreeRenderer.render(tree)

        logger.warn(s"Could not parse tree! \n ${rendered}")

        children.foreach(compile) // Compile anyways
      }
    }

  }

  /**
    * Puts the automaton in its initial state.
   **/
  private[this] def reset() : Unit = {

    nodeStack = Stack.empty[Node]

  }

  private[this] object Topic {

    def apply(label : String) : Option[Node] = label match {
      case "" => None
      case _ => {

        val topic = Option(model.getGraph.getNode(label)) match {
          case Some(topic) => topic
          case None => {

            val topic = model.factory.newNode(label)

            topic.getNodeData.setLabel(label)
            topic.getNodeData.setColor(0.5f, 0.5f, 0.5f)

            model.getGraph.addNode(topic)

            topic
          }
        }

        Some(topic)

      }

    }

  }

  private[this] object Predicate {

    def apply(source : Node, target : Node, label : String) : Edge = {

      val edge = model.factory.newEdge(source, target)

      edge.getEdgeData.setColor(0.5f, 0.5f, 0.5f)
      edge.getEdgeData.setLabel(label)

      model.getGraph.addEdge(edge)

      edge

    }
  }

  private[this] def propRules = {
    Set(("@S", "NP"))
  }

  private[this] def thatRules = {
    Set(("NP", "SBAR"), ("NP", "PP"), ("@NP", "SBAR"), ("@NP", "PP"))
  }

  private[this] def prepRules = {
    Set(("IN", "NP"), ("IN", "@NP"), ("IN", "VP"), ("IN", "S"))
  }

  private[this] def gerundRules = {
    Set(("VBG", "NP"), ("VBG", "@NP"))
  }

  private[this] def doubleRules = {
    Set(("VBZ", "VP"), ("VB", "VP"),
      ("VBD", "VP"), ("VBP", "VP"),
      ("VBG", "VP"), ("VBN", "VP"),
      ("TO", "VP"), ("MD", "VP"))
  }

  private[this] def predicateRules = {
    Set(("VB", "S"), ("VB", "NP"),
      ("VBD", "S"), ("VBD", "NP"),
      ("VBZ", "S"), ("VBZ", "NP"),
      ("VBP", "S"), ("VBP", "NP"),
      ("VBG", "S"), ("VBG", "NP"),
      ("VBN", "S"), ("VBN", "NP"))
  }

  private[this] def gateRules = {
    Set(("VB", "PP"), ("VB", "SBAR"),
      ("VBD", "PP"), ("VBD", "SBAR"),
      ("VBZ", "PP"), ("VBZ", "SBAR"),
      ("VBP", "PP"), ("VBP", "SBAR"),
      ("VBG", "PP"), ("VBG", "SBAR"),
      ("VBN", "PP"), ("VBN", "SBAR"))
  }

  private[this] def nestedRules = {
    Set(("@VP", "PP"), ("@VP", "SBAR"))
  }

}

/**
  * Compiles a graph from the text provided to STDIN, saving it to a GEXF file.
 **/
object Compiler {

   /**
     * Command line options for the tool.
    **/
   case class Config(
    file : File = null
  )

  /**
    * Set values for command line options. Specify
    * usage of the tool.
   **/
  val parser = new scopt.OptionParser[Config]("Compiler") {

    head("""Reads a block of text from STDIN. Compiles text into a topic map, capable of being
      viewed in the Gephi graph visualization software.""")

    opt[File]('f', "file").action {
      (x, c) => c.copy(file = x)
    }.text("file is a (GEXF) file property")

    help("help").text("Prints this help message.")

  }

  def main(args : Array[String]) : Unit = {

    parser.parse(args, Config()) map {
      cfg => {

        if(cfg.file != null) {

          val parser = new MemoizedParser

          val model = CreateGraphModel()

          val compile = new Compiler(model)
          val sentenceTrees = Blurb.tokens(System.in).map(parser.apply)

          sentenceTrees.foreach { tree => compile(tree) }

          val fs : FileOutputStream = new FileOutputStream(cfg.file)

          (new StaxGraphWriter).writeToStream(ToGexf(model), fs, "UTF-8")

        }


      }

    }

  }

}

