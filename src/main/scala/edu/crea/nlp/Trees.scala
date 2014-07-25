package edu.crea.nlp

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import scalaz._
import Scalaz._

package object Trees {

  import Isomorphism.<=>

  type BerkeleyTree[T] = edu.berkeley.nlp.syntax.Tree[T]

  implicit def isoTree[T] = new (BerkeleyTree[T] <=> Tree[T]) {

    val to : BerkeleyTree[T] => Tree[T] = { berkeleyTree =>

      val children = berkeleyTree.getChildren.asScala.toStream.map(to)

      children.size match {
        case 0 => berkeleyTree.getLabel.leaf
        case _ => Tree.node(berkeleyTree.getLabel, children)
      }

    }

    val from : Tree[T] => BerkeleyTree[T] = {
       case Tree.Node(label, Stream()) => new BerkeleyTree[T](label)
       case Tree.Node(label, subForest) => new BerkeleyTree[T](label, subForest.map(from).toList)
    }

  }

  implicit def toTree[T](berkeleyTree : BerkeleyTree[T]) : Tree[T] = isoTree.to(berkeleyTree)
  implicit def fromTree[T](tree : Tree[T]) : BerkeleyTree[T] = isoTree.from(tree)

  implicit def showTree[T] = new Show[Tree[T]] {

    override def shows(tree : Tree[T]) : String = {

      import edu.berkeley.nlp.syntax.Trees.PennTreeRenderer

      PennTreeRenderer.render(tree)

    }

  }

  implicit class TreeAdditions[T <% String](tree : Tree[T]) {

    def leaves : Stream[T] = tree match {
      case Tree.Node(terminal, Stream()) => Stream(terminal)
      case _ => tree.subForest.map(_.leaves).join
    }

    def id : String = {

      tree.leaves.flatMap(_.split(" "))
        .map(terminal => Lemmatizer(terminal.toLowerCase))
        .mkString(" ")
        .trim

    }

  }

}
