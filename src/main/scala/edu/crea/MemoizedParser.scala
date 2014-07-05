package edu.crea

import org.mapdb.DBMaker
import java.io.{ File, InputStream }
import edu.berkeley.nlp.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.syntax.Tree

import org.slf4j.{ Logger, LoggerFactory }

import scala.util.Try
import scala.collection.JavaConverters._
import TreeConversions._

class MemoizedParser(file : File = new File("database"), grammarFile : String = "eng_sm6.gr", verbose : Boolean = false) {

  private[this] val logger = LoggerFactory.getLogger(classOf[MemoizedParser])

  private[this] lazy val db = DBMaker.newFileDB(file).closeOnJvmShutdown.make()
  private[this] lazy val hashMap = db.getHashMap[String, Tree[String]]("hashMap")
  private[this] lazy val parser = new Parser(grammarFile)

  def apply(token : String) : Tree[String] = {
    val tree = Option(hashMap.get(token)) match {
      case Some(treeObj) => treeObj.asInstanceOf[Tree[String]]
      case None => {
        val tree = parser(token)
        hashMap.put(token, tree)
        db.commit()
        tree
      }
    }

    if(verbose) {

      val rendered = PennTreeRenderer.render(tree)
      logger.debug(s"${token}\n${rendered}")

    }

    tree
  }

}

