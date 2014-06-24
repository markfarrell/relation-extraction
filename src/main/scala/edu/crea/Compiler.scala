package edu.crea

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.{
  List, Set, Stack
}

import edu.berkeley.nlp.syntax.Tree

import java.io.File
import java.io.FileOutputStream

import org.slf4j.{Logger, LoggerFactory}
import edu.berkeley.nlp.syntax.Trees.PennTreeRenderer

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

    compile(tree)
    reset()
    model

  }

  object PredicateArgument {

    private [this] def nounAdjunctRules = {
      Set(("NN", "NNS"), ("NN", "NN"), ("NN", "NNPS"),
        ("NNP", "NNS"), ("NNP", "NN"), ("NNP", "NNPS"))
    }

    def unapply(tree : LinguisticTree) : Boolean = {

      tree.getLabel match {
        case "@NP" | "NP" if tree.existsBinaryRules(nounAdjunctRules) => true
        case "NN" | "NNS" | "NNP" | "NNPS" => true
        case _ => false
      }

    }

  }

  object MonovalentPredicate {

    def unapply(tree : LinguisticTree) : Boolean = {

      tree.getLabel match {
        case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => true
        case _ => false
      }

    }

  }

  object DivalentPredicate {

    private[this] def divalentRules = {
      Set(("VB", "S"), ("VB", "NP"), ("VB", "PP"), ("VB", "SBAR"),
        ("VBD", "S"), ("VBD", "NP"), ("VBD", "PP"), ("VBD", "SBAR"),
        ("VBP", "S"), ("VBP", "NP"), ("VBP", "PP"), ("VBP", "SBAR"),
        ("VBG", "S"), ("VBG", "NP"), ("VBG", "PP"), ("VBG", "SBAR"),
        ("VBN", "S"), ("VBN", "NP"), ("VBN", "PP"), ("VBN", "SBAR"))
    }

    def unapply(tree : LinguisticTree) : Option[Tuple2[LinguisticTree, LinguisticTree]] = {

      tree.getLabel match {
        case "@VP" | "VP" | "PP" => tree.findBinaryRules(divalentRules)
        case _ => None
      }

    }

  }

  object TrivalentPredicate {

    private[this] def trivalentRules = Set(("@VP", "NP"))

    def unapply(tree : LinguisticTree) : Option[Tuple2[LinguisticTree, LinguisticTree]] = {
      tree.getLabel match {
        case "@VP" | "VP" => tree.findBinaryRules(trivalentRules)
        case _ => None
      }
    }

  }

  object NonfiniteVerbPhrase {

    private [this] def nonfiniteVerbRules = {
      Set(("VBZ", "VP"), ("VB", "VP"),
        ("VBD", "VP"), ("VBP", "VP"),
        ("VBG", "VP"), ("VBN", "VP"),
        ("TO", "VP"), ("MD", "VP"))
    }

    def unapply(tree : LinguisticTree) : Option[Tuple2[LinguisticTree, LinguisticTree]] = {

      tree.getLabel match {
        case "@VP" | "VP" => tree.findBinaryRules(nonfiniteVerbRules)
        case _ => None
      }

    }

  }

  object NounWithPreposition {

    private [this] def nounWithPrepositionRules = {
      Set(("NP", "PP"))
    }

    // TODO: Discard left tree, compile right
    def unapply(tree : LinguisticTree) : Option[Tuple2[LinguisticTree, LinguisticTree]] = tree.getLabel match {
      case "@NP" | "NP" => {
        tree.findBinaryRules(nounWithPrepositionRules)
      }
      case _ => None
    }

  }

  object IgnoredConstituent {

    def unapply(tree : LinguisticTree) : Boolean = {

      tree.getLabel match {
        case "ADVP" | "X" | "@X" | "NX" | "@NX" | "DT" | "JJ" | "JJS" | "JJR" | "-LRB-" | "-RRB-" => true
        case _ => false
      }

    }

  }

  object IfThenClause {

    private[this] def ifThenRules = Set(("@S", "NP"))

    def unapply(tree : LinguisticTree) : Option[Tuple2[LinguisticTree, LinguisticTree]] = {

      tree.getLabel match {
        case "@S" | "S" => {
          tree.findBinaryRules(ifThenRules)
        }
        case _ => None
      }

    }

  }

  private[this] def compile(tree : LinguisticTree) : Unit = {

    val children = tree.getChildren.asScala.toList

    tree match {
      case TrivalentPredicate(left, right) => {
        compile(left) // John gave Jill
        compile(right) // His dog
        compile(left) // His dog gave (was given to) Jill
      }
      case DivalentPredicate(left, right) => {

        val sourceOption = nodeStack.headOption

        compile(right) // Verb

        val targetOption = nodeStack.headOption
        val label = left.terminalValue

        for {
          source <- sourceOption
          target <- targetOption
        } CreateEdge(source, target, label)

      }
      case MonovalentPredicate() => {

        val label = tree.terminalValue

        for {
          source <- nodeStack.headOption
        } CreateEdge(source, source, label)

      }
      case PredicateArgument() => {

        for (node <- CreateNode(tree.terminalValue)) {
          nodeStack = nodeStack.push(node)
        }

      }
      case NounWithPreposition(_, right) => compile(right)
      case NonfiniteVerbPhrase(_, right) => compile(right)
      case IgnoredConstituent()  => ()
      case _ => children.foreach(compile) // compile(left) ... compile(right)
    }

  }

  /**
    * Puts the automaton in its initial state.
   **/
  private[this] def reset() : Unit = {

    nodeStack = Stack.empty[Node]

  }

  private[this] object CreateNode {

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

  private[this] object CreateEdge {

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

  private[this] def createNounRules = {
    Set(("NN", "NNS"), ("NN", "NN"), ("NN", "NNPS"),
      ("NNP", "NNS"), ("NNP", "NN"), ("NNP", "NNPS"))
  }

  private [this] def doubleNounRules = {
    Set(("@NP", "NP"), ("@NP", "NN"), ("@NP", "NNS"),
     ("@NP", "NNP"), ("@NP", "NNPS"), ("@NP", "S"),
     ("NP", "NP"), ("NN", "S"), ("NNP", "S"),
     ("NNS", "S"), ("NNPS", "S"))
  }

  private[this] def doubleVerbRules = {
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

  // E.g. the man might hunt the dog if the cat could eat.
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

