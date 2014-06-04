package edu.berkeley.crea.beagle

import org.mapdb.DBMaker
import java.io.{ File, InputStream }

import scala.util.Try
import scala.collection.JavaConverters._
import TreeConversions._

class MemoizedParser(file : File = new File("database")) {

  private[this] lazy val db = DBMaker.newFileDB(file).closeOnJvmShutdown.make()
  private[this] lazy val hashMap = db.getHashMap[String, LinguisticTree]("hashMap")
  private[this] lazy val parser = new Parser

  def read(in : InputStream, treeFunction : (LinguisticTree) => Any = { _ => () }) = Tokenize(in) {
    token => treeFunction(apply(token))
  }

  def apply(token : String) : LinguisticTree = Option(hashMap.get(token)) match {
    case Some(tree) => tree.asInstanceOf[LinguisticTree]
    case None => {
      val tree = parser(token)
      hashMap.put(token, tree)
      db.commit()
      tree
    }
  }


}

object MemoizedParser {

  def main(args : Array[String]) : Unit = {

    val memoizedParser = {
      Try(new MemoizedParser(new File(args(0)))).getOrElse(new MemoizedParser)
    }

    memoizedParser.read(System.in)

  }
}
