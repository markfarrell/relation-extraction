package edu.crea

import java.io.{File, InputStream, FileOutputStream}

import org.gephi.graph.api.GraphModel
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

object Compiler {

  def apply(inputStream : InputStream, verbose : Boolean) : GraphModel = {

    import Patterns._

    val parser = new MemoizedParser(verbose = verbose)

    val model = CreateGraphModel()

    Tokenize(inputStream)
      .map(parser.apply)
      .map(convertTree)
      .map(RootExpression.apply)
      .iterator
      .collect {
        case Some(clauses) => ToGraph(clauses, model)
      }

    model

  }

}

object CompilerApp extends App {

  /**
    * Command line options for the tool.
   **/
  case class Config(file : File = null, verbose : Boolean = false)

  /**
   * Set values for command line options. Specify
   * usage of the tool.
   **/
  val optionParser = new scopt.OptionParser[Config]("Compiler") {

    head("""Reads a block of text from STDIN. Compiles text into a text-network.""")

    opt[File]('f', "file").action {
      (x, c) => c.copy(file = x)
    }.text("file is a (GEXF) file property")

    opt[Unit]('v', "verbose").action {
      (_, c) => c.copy(verbose = true)
    }.text("flag for printing debug messages")

    help("help").text("Prints this help message.")

  }

  optionParser.parse(args, Config()) map { cfg =>

    if(cfg.file != null) {

      val fs : FileOutputStream = new FileOutputStream(cfg.file)

      (new StaxGraphWriter).writeToStream(ToGexf(Compiler(System.in, cfg.verbose)), fs, "UTF-8")

      fs.close()

    }

  }

}


