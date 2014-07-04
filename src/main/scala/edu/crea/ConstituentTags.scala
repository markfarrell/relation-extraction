package edu.crea

import scalaz._
import Scalaz._

import scala.collection.immutable.Stream
import scala.collection.JavaConverters._

package object ConstituentTags {

  implicit class ConstituentTagTree(javaTree : edu.berkeley.nlp.syntax.Tree[String]) {

    def toConstituentTagTree : Tree[String] = {

      val children = javaTree.getChildren.asScala.toStream.map(_.toConstituentTagTree)

      children.size match {
        case 0 => javaTree.getLabel.leaf
        case _ => Tree.node(javaTree.getLabel, children)
      }

    }
  }

}



