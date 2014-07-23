package edu.crea

import org.mapdb.DBMaker
import java.io.File

import scalaz._
import Scalaz._

import scala.util.{Try, Success, Failure}

class MemoizedParser(file : File = new File("database")) {

  import Trees._

  private[this] lazy val db = DBMaker.newFileDB(file).closeOnJvmShutdown.make()
  private[this] lazy val parseMap = db.getTreeMap[String, BerkeleyTree[String]]("parses")
  private[this] lazy val parser = new Parser

  def apply(token : String) : Tree[String] = {

    val tree = Try(Option(parseMap.get(token))) match {

      case Success(Some(treeObj)) =>

        val tree : Tree[String] = treeObj.asInstanceOf[BerkeleyTree[String]]

        tree

      case _ =>

        val tree = parser(token)
        val berkeleyTree : BerkeleyTree[String] = tree

        Try {
          parseMap.put(token, berkeleyTree)
          db.commit()
        }

        tree

    }

    tree

  }

}

