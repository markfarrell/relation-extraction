package edu.crea

import java.io.{File, InputStream, OutputStream, FileOutputStream}

import org.gephi.graph.api.GraphModel
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

object Compiler {

  private[this] lazy val parser = new MemoizedParser
  private[this] lazy val model = CreateGraphModel()
  private[this] val charset = "UTF-8"

  def apply(inputStream : => InputStream, outputStream : => OutputStream) : Float = {

    import Patterns._

    val writer = new StaxGraphWriter
    var (success, fail) = (0f, 0f)

    def tokens = Tokenize(inputStream)

    def clauseLists = {
      tokens.map(parser.apply)
        .map(convertTree)
        .map(RootExpression.apply)
    }

    clauseLists.iterator.foreach {
      case Some(clauses) => {

        ToGraph(clauses, model)
        success += 1f

      }
      case None => fail += 1f
    }

    writer.writeToStream(ToGexf(model), outputStream, charset)

    fail/(fail+success)

  }

}

object CompilerApp extends App {

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

      val rate = Compiler(System.in, fs)

      if(rate > 0f) {
        val result = "%.3f".format(rate)
        println(s"Fail rate: ${result}")
      }

      fs.close()

    }

  }

}


