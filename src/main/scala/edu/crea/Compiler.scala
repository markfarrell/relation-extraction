package edu.crea

import java.io.{File, InputStream, OutputStream, FileInputStream, FileOutputStream}

import org.gephi.graph.api.GraphModel
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter

object Compiler {

  import Patterns._
  import scalaz._
  import Scalaz._

  private[this] lazy val parser = new MemoizedParser
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

        case None => fail += 1f
      }

    writer.writeToStream(ToGexf(model), outputStream, charset)

    fail/(fail+success)

  }

  def fails(file : File) : Stream[String] = {

    def fs = new FileInputStream(file)

    def tokens = Tokenize(fs)

    def pairs = tokens.zip(tokens.map(parser.apply).map(RootExpression.apply))

    val it = pairs.filter(_._2 === none).map(_._1).iterator

    def stream : Stream[String] = if(it.hasNext) {
      it.next #:: stream
    } else {
      fs.close()
      Stream.empty[String]
    }

    stream

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


