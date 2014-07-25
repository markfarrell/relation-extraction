package edu.crea.nlp

import java.io.{File, ByteArrayInputStream, InputStream, FileInputStream}

import scalaz._
import Scalaz._

import Patterns._
import Terms._

object Compile {

  private[this] lazy val parser = new Parser

  def apply(str : String) : Stream[Compound] = apply(new ByteArrayInputStream(str.getBytes))

  def apply(file : File) : Stream[Compound] = {

    val fs = new FileInputStream(file)

    val res = apply(fs)

    fs.close()

    res
  }

  def apply(inputStream : => InputStream) : Stream[Compound] = {

    def trees = Tokenize(inputStream).map(parser.apply)

    trees.flatMap(RootExpression.apply).join

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
