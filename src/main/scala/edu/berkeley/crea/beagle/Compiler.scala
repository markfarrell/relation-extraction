package edu.berkeley.crea.beagle

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.{
  List, Set, Stack
}

import edu.berkeley.nlp.syntax.Tree

import java.io.File
import java.io.FileOutputStream

import org.gephi.streaming.server.StreamingServer
import org.gephi.streaming.server.impl.ServerControllerImpl
import org.gephi.streaming.server.impl.jetty.StreamingServerImpl

import org.gephi.graph.api.{ Graph, Node, Edge, GraphFactory, GraphModel }

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

import TreeConversions._

/**
  * A Compiler front-end that writes to an intermediate in-memory graphstore,
  * whose contents can then be written to a GEXF file or database back-end.
  * @author Mark Farrell
 **/
class Compiler(model : GraphModel) {

  private[this] var topicStack : Stack[Node] = Stack.empty[Node]
  private[this] var gateStack : Stack[Edge] = Stack.empty[Edge]

  /**
    * Returns the GraphModel passed as an argument to the Compiler's
    * constructor, after it has been mutated with new nodes and edges added.
    * @param tree - The parts-of-speech tagged tree to compile into a graph.
    * @return The GraphModel used by the compiler.
   **/
  def apply(tree : LinguisticTree) : GraphModel = {

    model.getGraph.writeLock()

    compileTopics(tree)

    model.getGraph.writeUnlock()

    clear()
    model
  }

  /**
    * Clears all collections stored as fields in the compiler.
    * Used internally to reset the state of compiler after each
    * sentence is parsed and compiled.
   **/
  private[this] def clear() : Unit = {

    topicStack = Stack.empty[Node]
    gateStack = Stack.empty[Edge]

  }

  /**
    * Used for building topic nodes and adding them to the graph.
   **/
  private[this] object Topic {

    /**
      * Either builds a new node and adds it to the graph
      * or returns an existing topic node, since they are unique.
      * @param label - The unique label to give to the topic node.
      * @return The topic node, whether it has been created or fetched
      * as an existing node in the graph.
     **/
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

  /**
    * Used for building edges and adding them to the graph.
   **/
  private[this] object Predicate {

    def apply(source : Node, target : Node, label : String) : Edge = {

      val edge = model.factory.newEdge(source, target)

      edge.getEdgeData.setColor(0.5f, 0.5f, 0.5f)
      edge.getEdgeData.setLabel(label)

      model.getGraph.addEdge(edge)

      edge

    }
  }

  /**
    * Mutates gateStack, pushing onto it new edges that were created on the graph in this method
    * call. Forms relations between topic nodes with labels it extracts from prepositional phrases
    * and subordinate conjunctions found in the <code>tree</code> parameter.
    * @param tree - A parts-of-speech tagged tree, labelled as <code>PP</code> or <code>SBAR</code>.
    * @param sourceOption - Either specify some source topic node for all edges created in this method
    * call or specify none, in which case looping edges will be formed for all target topic nodes found
    * in <code>tree</code>'s children.
   **/
  private[this] def compileGates(tree : LinguisticTree, sourceOption : Option[Node]) : Unit = {

    // Filters the source node: edge loops are not wanted here.
    def ok(target : Node) : Boolean = sourceOption match {
      case Some(source) => source.getNodeData.getLabel != target.getNodeData.getLabel
      case None => false
    }

    val children = tree.getChildren.asScala

    tree.getLabel match {
      case "PP" | "SBAR" if children.size == 2 => {

        val (left, right) = (children.head, children.last)

        val label = left.terminalValue

        compileTopics(right)

        for {
          target <- topicStack if ok(target)
          source <- sourceOption
        } {

          gateStack = gateStack.push(Predicate(source, target, label))

        }

      }
      case "SBAR" if children.size == 1 => {

        val child = children.head
        val label = ""

        compileTopics(child)

        for {
          target <- topicStack
          source <- sourceOption
        } {

          gateStack = gateStack.push(Predicate(source, target, label))

        }

      }
      case _ => {

        import org.slf4j.{Logger, LoggerFactory}
        import edu.berkeley.nlp.syntax.Trees.PennTreeRenderer

        val logger = LoggerFactory.getLogger(classOf[Compiler])
        val rendered = PennTreeRenderer.render(tree)

        logger.warn(s"Could not parse tree! \n ${rendered}")

      }
    }

  }

  /**
    * Creates edges between topic nodes that have already been added to the graph.
    * Also allows more topic nodes to be added to the graph. Mutates gateStack and
    * topicStack.
    * @param tree - A parts-of-speech annotated tree to match on.
   **/
  private[this] def compileArrows(tree : LinguisticTree) : Unit = {

    // VerbPreterminal ::= VB | VBD | VBZ | VBP | VBG | VBN
    // PredicateRule ::= VerbPreterminal . S | NP
    // DoubleRule ::= VerbPreterminal | MD | TO . VP
    // GateRule ::= VerbPreterminal . PP | SBAR
    // NestedRule ::=  @VP . PP | SBAR

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

    def nestedRules = {
      Set(("@VP", "PP"), ("@VP", "SBAR"))
    }

    def updateGates(headOption : Option[Node], right : LinguisticTree) : Unit = {

        var targetGates = gateStack

        gateStack = Stack.empty[Edge]
        compileGates(right, headOption)

        targetGates ++= gateStack

        for {
          source <- headOption
          targetGate <- targetGates
        } {

          val target = targetGate.getSource

          val label = {
            Option(targetGate.getEdgeData.getLabel).getOrElse("")
          }

          Predicate(source, target, label)

        }
    }

    tree.getLabel match {
      case "@VP" | "VP" | "PP" if tree.existsBinaryRules(predicateRules) => {

        val (left, right) = tree.findBinaryRules(predicateRules).get

        val lastOption = topicStack.lastOption

        compileTopics(right)

        val label = left.terminalValue
        val targets = topicStack

        for {
          source <- lastOption
          target <- targets.headOption
        } Predicate(source, target, label)

      }
      case "@VP" | "VP" | "PP" if tree.existsBinaryRules(nestedRules) => {

        val (left, right) = tree.findBinaryRules(nestedRules).get

        val headOption = topicStack.headOption

        compileArrows(left)

        updateGates(headOption, right)

      }
      case "@VP" | "VP" | "PP" if tree.existsBinaryRules(gateRules) => {

        val (left, right) = tree.findBinaryRules(gateRules).get

        val headOption = topicStack.headOption
        val label = left.terminalValue

        for(source <- headOption) {
          Predicate(source, source, label)
        }

        updateGates(headOption, right)

      }
      case "@VP" | "VP" if tree.existsBinaryRules(doubleRules) => {

        val (_, right) = tree.findBinaryRules(doubleRules).get

        compileArrows(right)

      }
      case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => {

        val label = tree.terminalValue

        for {
          source <- topicStack.headOption
        } Predicate(source, source, label)

      }
      case "@NP" | "NP" => compileTopics(tree)
      case _ => for(c <- tree.getChildren.asScala) {
        compileArrows(c)
      }
    }

  }

  /**
    * Mutates topicStack, pushing onto it new topic nodes that were adding
    * to the graph during this method call.
    * @param tree - The parts-of-speech annotated tree to match on.
   **/
  private[this] def compileTopics(tree : LinguisticTree) : Unit = {

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
      case "@NP" | "NP" if !tree.existsBinaryRules(thatRules) => {

        for (topic <- Topic(tree.terminalValue)) {

          topicStack = topicStack.push(topic)

          for(gate <- gateStack.headOption) {

            val source = gate.getSource

            val label = {
              Option(gate.getEdgeData.getLabel).getOrElse("")
            }

            Predicate(source, topic, label)

          }

        }

      }
      case "@VP" | "VP" if tree.existsBinaryRules(gerundRules) => {

        val (left, right) = tree.findBinaryRules(gerundRules).get

        for (topic <- Topic(right.terminalValue)) {
          topicStack = topicStack.push(topic)
        }

        compileArrows(left) // Makes topic connect to itself

      }
      case "@VP" | "VP" => {

        compileArrows(tree)

      }
      case "@NP" | "NP" if tree.existsBinaryRules(thatRules) => {

        val (left, right) = tree.findBinaryRules(thatRules).get

        val headOption = topicStack.headOption
        compileTopics(left)

        // Connects to previously added topics
        compileGates(right, headOption)

      }
      case "SBAR" | "PP" if tree.existsBinaryRules(propRules) => {

        val headOption = topicStack.headOption

        compileGates(tree, headOption)

      }
      case _ => for(c <- tree.getChildren.asScala) {
        compileTopics(c)
      }
    }

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

