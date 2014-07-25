package edu.crea.gexf

import java.io.{File, InputStream, OutputStream, FileInputStream, FileOutputStream}

import org.slf4j.{LoggerFactory, Logger}

import org.gephi.graph.api.GraphModel
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

import scalaz._
import Scalaz._

import edu.crea.nlp.{Tokenize, Parser, Compile}
import edu.crea.nlp.Terms._
import edu.crea.nlp.Patterns._

object GexfCompiler {

  private[this] lazy val parser = new Parser
  private[this] lazy val model = CreateGraphModel()
  private[this] val charset = "UTF-8"
  private[this] val cores = 4

  def apply(inputStream : => InputStream, outputStream : => OutputStream) : Float = {

    val writer = new StaxGraphWriter
    var (success, fail) = (0f, 0f)

    def tokens = Tokenize(inputStream)

    // TODO: Compilation can be memoized.

    tokens.map(parser.apply)
      .grouped(cores)
      .flatMap(group => group.par.map(RootExpression.apply))
      .foreach {
        case Some(clauses) =>

          ToGraph(clauses, model)
          success += 1f

        case None =>

          fail += 1f

      }

    writer.writeToStream(ToGexf(model), outputStream, charset)

    success/(fail+success)

  }

}

object GexfCompilerApp extends App {

  private[this] val logger = LoggerFactory.getLogger(GexfCompilerApp.getClass)

  /**
    * Command line options for the tool.
   **/
  case class Config(file : File = null)

  /**
   * Set values for command line options. Specify
   * usage of the tool.
   **/
  val optionParser = new scopt.OptionParser[Config]("Compiler") {

    head("""Reads a block of text from STDIN. Compiles text into a text-network.""")

    opt[File]('f', "file").action {
      (x, c) => c.copy(file = x)
    }.text("file is a (GEXF) file property")

    help("help").text("Prints this help message.")

  }

  optionParser.parse(args, Config()) map { cfg =>

    if(cfg.file != null) {

      val fs : FileOutputStream = new FileOutputStream(cfg.file)

      val rate = GexfCompiler(System.in, fs) * 100f

      if(rate > 0f) {

        val result = "%.3f".format(rate)

        logger.debug(s"Success rate: ${result}%")

      }

      fs.close()

    }

  }

}


