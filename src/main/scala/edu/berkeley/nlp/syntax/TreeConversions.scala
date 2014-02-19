package edu.berkeley.nlp.syntax

import java.util.Calendar

import scala.language.implicitConversions
import scala.language.postfixOps

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.mutable.Stack

import it.uniroma1.dis.wsngroup.gexf4j.core.EdgeType
import it.uniroma1.dis.wsngroup.gexf4j.core.Gexf
import it.uniroma1.dis.wsngroup.gexf4j.core.Graph
import it.uniroma1.dis.wsngroup.gexf4j.core.Mode
import it.uniroma1.dis.wsngroup.gexf4j.core.Node
import it.uniroma1.dis.wsngroup.gexf4j.core.data.Attribute
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeClass
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeList
import it.uniroma1.dis.wsngroup.gexf4j.core.data.AttributeType
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.GexfImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.data.AttributeListImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.viz.NodeShape

package object TreeConversions {

  type LinguisticTree = Tree[String]
  implicit def TreeToTreeEnhancer(tree : LinguisticTree) = new TreeEnhancer(tree)

  /** 
    * Methods added to Tree through implicit conversions with this class.
   **/
  class TreeEnhancer(tree : LinguisticTree) { 

    /**
      * @method findCut
      * @param labels
      * @return The longest slice at any level of the tree containing only the labels provided
      * as an argument to this function. 
      **/
    def findCut(labels : Set[String]) : Option[LinguisticTree] = (axed()  filter { 
        (t : LinguisticTree) => { (t.getChildren().asScala forall { 
            (child : LinguisticTree) => labels contains { child.getLabel() }
          }) || ( labels contains {
            t.getLabel() 
          }) 
        }
      }).toList.sortWith { 
        _.totalSize() > _.totalSize() 
      } find { 
        (_) => true
      } 

    /** 
      * @method totalSize
      * @return The number of nodes in the tree. 
     **/
    def totalSize() = tree.iterator().asScala.length

    /** 
      * @method replaceChildren
      * @param s
      * @return - A new tree with the original root node of the tree,
      * but with children set to s.
     **/
    def replaceChildren(s : Set[LinguisticTree]) : LinguisticTree = { 
      val root : LinguisticTree = tree.shallowCloneJustRoot()
      root.setChildren( s.toList.asJava )
      root
    }

    /** 
      * @method slices - there should be (n^2/2 + n) slices for a list of 
      * children of size n.
      * @return All possible slices of a node's children.
    **/
    def slices() : Set[Set[LinguisticTree]] = { 
      val ls : Set[LinguisticTree] = tree.getChildren().asScala.toSet[LinguisticTree] 
      ((0 to (ls.size + 1)) flatMap { 
        (x : Int) => (0 to (ls.size+1-x)) map { 
          (y : Int) => ls.slice(y, (ls.size+1)-x).toSet[LinguisticTree]
        }
      }).toSet[Set[LinguisticTree]] filter { _.size > 0 } 
    }

    /** 
      * @method axed  
      * @return Trees for all possible slices at each level of the tree.
      **/
    def axed() : Set[LinguisticTree] = {

      if(tree.isPreTerminal() || tree.getChildren().size() == 0) {
        Set[LinguisticTree](tree)
      } else {  

        val childSlices : Set[Set[LinguisticTree]] = slices() 
        val flattened : Set[LinguisticTree] = childSlices flatten
        val prepended : Set[LinguisticTree] = childSlices map { replaceChildren(_) }
        val powered : Set[LinguisticTree] = flattened flatMap { _.axed() } 

        prepended | powered

      }

    }

    /** 
      * @method terminalList
      * @return - The list of terminals constructed when the leaf nodes of
      * the tree are followed from left to right.
     **/
    def terminalList() : List[String] =  tree.iterator().asScala.foldLeft(List[String]()) {
      (ls, child) => if(child.isPreTerminal()) ls ++ List[String](child.getChild(0).getLabel())  else ls
    } 

  }

  /** 
   * Implicitly convert a tree to a list of edges when this object's
   * members are imported. 
   * 
   * Example Usage:
   *    import TreeImplicits._
   *    val edgeList : List[(String, String)] = tree 
   **/
  implicit def TreeToEdgeList(tree : Tree[_]) : List[(String, String)] = { 

    tree.iterator().asScala flatMap { 
      (t1 : Tree[_])  => {

        t1.getChildren().asScala filterNot {
          _.getLabel() == null
        } map { 
          (t2: Tree[_])  => (t1.getLabel().toString(), t2.getLabel().toString())
        }

      }

    } toList

  }

  /** 
   * Implicitly convert a string containing an s-expression, valid according to Penn Treebank guidelines,
   * to a Tree with string labels.
   **/
  implicit def StringToTree(str : String) : Tree[String] = Trees.PennTreeReader.parseEasy(str, false)

} 
