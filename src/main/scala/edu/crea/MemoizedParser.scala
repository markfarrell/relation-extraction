package edu.crea

import org.mapdb.DBMaker
import java.io.File

import scalaz._
import Scalaz._

class MemoizedParser(file : File = new File("database")) {

  import Trees._

  private[this] lazy val db = DBMaker.newFileDB(file).closeOnJvmShutdown.make()
  private[this] lazy val hashMap = db.getHashMap[String, BerkeleyTree[String]]("parses")
  private[this] lazy val parser = new Parser

  def apply(token : String) : Tree[String] = {

    val tree = Option(hashMap.get(token)) match {

      case Some(treeObj) =>

        val tree : Tree[String] = treeObj.asInstanceOf[BerkeleyTree[String]]

        tree

      case None =>

        val tree = parser(token)
        val berkeleyTree : BerkeleyTree[String] = tree

        hashMap.put(token, berkeleyTree)
        db.commit()

        tree

    }

    tree

  }

}

