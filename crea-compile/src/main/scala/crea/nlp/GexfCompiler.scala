package crea.nlp

import java.io.{File, FileOutputStream}

object GexfCompiler extends App {

  case class Config(file : File = null)

  val optionParser = new scopt.OptionParser[Config]("Compiler") {

    head("""Compiles text into a graph.""")

    opt[File]('f', "file").action {
      (x, c) => c.copy(file = x)
    }.text("file is a (GEXF) file property")

    help("help").text("Prints this help message.")

  }

  optionParser.parse(args, Config()) map { cfg =>

    if(cfg.file != null) {

      val fs : FileOutputStream = new FileOutputStream(cfg.file)

      val document = new Document(System.in)

      document.compiles.to(Document.gexf(fs)).run.run

      fs.close()

    }

  }

}


