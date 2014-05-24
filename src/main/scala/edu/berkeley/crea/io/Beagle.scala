package edu.berkeley.crea.io

import java.io.File
import java.io.FileOutputStream

import scala.collection.mutable.{HashMap, MultiMap, Set}

import edu.berkeley.nlp.syntax.Tree

import edu.berkeley.crea.syntax.TreeConversions._

import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter



/**
  * @object Beagle - A tool for exporting environments to Postgres databases,
  * clearing databases, reloading environments saved in databases and compiling
  * environments to Gephi datasets.
 **/
object Beagle {

   /**
     * @class Config - command line options for the tool.
    **/
   case class Config(
    file : File = null,
    grammar : String = "./lib/eng_sm6.gr"
  )

  /**
    * Set values for command line options. Specify
    * usage of the tool.
   **/
  val parser = new scopt.OptionParser[Config]("Beagle") {

    head(
      """Beagle ---
      Reads a block of text from STDIN. Compiles text into a topic map, capable of being
      viewed in the Gephi graph visualization software."""
    )

    opt[File]('f', "file") action {
      (x, c) => c.copy(file = x)
    } text("file is a (GEXF) file property")

    opt[String]('g', "grammar") action {
      (x, c) => c.copy(grammar = x)
    } text("grammar is a string (path) property")

    help("help") text("Prints this help message.")

  }


  /**
    * @method main - Entry point for the tool.
    * @param args - {Array[String]}
   **/
  def main(args : Array[String]) : Unit = {

    parser.parse(args, Config()) map {
      cfg => {

        if(cfg.file != null) {

          import org.gephi.graph.store.GraphModelImpl
          import edu.berkeley.crea.syntax.{ Compiler, ToGexf }

          val parser : DefaultParser = new DefaultParser(cfg.grammar)

          def parse(str : String) : Tree[String]  = {
            val ret : Tree[String] = parser.parse(str)
            println(str + " -> " + ret.toString)
            ret
          }

          val model = new GraphModelImpl
          val sentenceTrees = Blurb.tokens(System.in).map(parse)

          sentenceTrees.foreach { tree => (new Compiler()(model))(tree) }

          val fs : FileOutputStream = new FileOutputStream(cfg.file)

          (new StaxGraphWriter).writeToStream(ToGexf(model), fs, "UTF-8")

        }


      }

    }

  }

}
