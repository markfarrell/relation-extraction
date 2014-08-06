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

  private[this] def parse(token : String) : Tree[String] = parser(token)

  private[this] def compile(tree : Tree[String]) : Tree[String] \/ Stream[Compound] = RootExpression(tree) match {
    case Some(stream) => stream.right
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

  lazy val parses : Process[Task, Tree[String]] = tokens.map(parse)

  lazy val compiles : Process[Task, Tree[String] \/ Stream[Compound]] = parses.map(compile)

}
