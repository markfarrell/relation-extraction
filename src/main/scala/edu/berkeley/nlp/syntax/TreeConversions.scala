package edu.berkeley.nlp.syntax

import java.util.Calendar

import scala.language.implicitConversions
import scala.language.postfixOps

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.List

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

object TreeConversions { 

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

  /** 
   * Converts a tree into a civilized GEXF object, capable of being viewed as a graph in Gephi.
   **/
  implicit def TreeToGEXF(tree : Tree[String]) : Gexf = {

    // TODO: Build list of graph nodes, not list of string; set properties based on state.
    val transform : (Tree[String]) => List[String] = (tree) => {

      tree.iterator().asScala.foldLeft((List[String](), 0)) { 
        (state, node) => {

          val list : List[String] = state._1
          val current : Int = state._2

          node.getLabel() match { 
            case "S" => (list, current)
            case "SBAR" => (list, current) 
            case "IN" => (if(node.isPreTerminal()) node.getChild(0).getLabel() :: list else list, current)
            case "NP" => ({

                val result : String = node.iterator().asScala.foldLeft("") {
                  (str, child) => if(child.isPreTerminal()) str + " " + child.getChild(0).getLabel()  else str
                } 

                if(result.length > 0) result :: list else list 

              }, current)
            case "VP" => {

                //Iterate over children: find and add modals, VB, VBZ, VBP, VBD, VBN or VBG.
                val result : (String, Int) = node.iterator().asScala.foldLeft(("", current)) { 
                  (concatState, child) => {

                    if(child.isPreTerminal()) {

                      val terminalLabel : String = child.getChild(0).getLabel()

                      child.getLabel() match {
                        case "MD" => (concatState._1  + " " + terminalLabel, 1)
                        case _ => (concatState._1 + " " + terminalLabel, concatState._2)
                    } 
                  } else { 
                    concatState
                  } 

                }

              }

              (result._1 :: list, result._2)


            }
            case _ => (list, current)
          }

        }

      }._1.reverse

    }

        
        
     val gexf : Gexf = new GexfImpl();

     gexf.getMetadata()
         .setLastModified(Calendar.getInstance().getTime())
         .setCreator("Civilize")
         .setDescription("Useful representation of a linguistic tree")

     gexf.setVisualization(true)

     val graph : Graph = gexf.getGraph()
     graph.setDefaultEdgeType(EdgeType.UNDIRECTED).setMode(Mode.STATIC)

     val attrList : AttributeList = new AttributeListImpl(AttributeClass.NODE)
     graph.getAttributeLists().add(attrList)

     

     gexf
  }

} 
