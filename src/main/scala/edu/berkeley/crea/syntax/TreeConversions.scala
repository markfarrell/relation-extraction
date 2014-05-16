package edu.berkeley.crea.syntax

import java.util.Calendar

import edu.berkeley.nlp.syntax.{ Tree, Trees } 

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
   
  /** 
   * @method StringToTree - Implicitly convert a string containing an s-expression, valid according to Penn Treebank guidelines,
   * to a Tree with string labels.
   * @param str {String} 
   * @return {LinguisticTree}
   **/
  implicit def StringToTree(str : String) : LinguisticTree = Trees.PennTreeReader.parseEasy(str, false)

  /** 
    * @class TreeEnhancer
    * @param tree {LinguisticTree}
   **/
  implicit class TreeEnhancer(tree : LinguisticTree) { 

    /**
      * @method terminalValue
      * @return {String} 
     **/ 
    def terminalValue : String = {

      def terminalLabels(tree : LinguisticTree) : String = {
        tree.getTerminals.asScala map { 
          _.getLabel 
        } mkString("") 
      } 

      val str = tree.iterator.asScala.toList filter { 
        _.isPreTerminal
      } filter { 
        _.getLabel match { 
          case "PDT" | "DT" | "PRP$" => false
          case _ => true
        } 
      } map {  
        t => Lemmatizer.lemmatize(terminalLabels(t)) 
      } mkString(" ")

      str.toLowerCase.replaceAll("[.!?]", "")

    } 

    /**
      * @method existsBinaryRules
      * @param pairs {Set[(String, String)]} 
      * @return {Boolean} 
      **/
    def existsBinaryRules(pairs : Set[(String, String)]) : Boolean  = {

      pairs exists { 
        rule : (String, String) => tree.existsBinaryRule(rule) 
      } 

    }

    /**
      * @method existsBinaryRule
      * @param rule {(String, String)}
      * @return {Boolean} 
      **/
    def existsBinaryRule(rule : (String, String)) : Boolean = { 

      tree.findBinaryRule(rule) match { 
        case Some(_) => true
        case None => false 
      } 

    } 

    /** 
      * @method findBinaryRule
      * @param rule {(String, String)} 
      * @return {Option[(LinguisticTree, LinguisticTree)]}  
      **/
    def findBinaryRule(rule : (String, String)) : Option[(LinguisticTree, LinguisticTree)] = { 

      tree.getChildren.asScala.toList match {
        case List(a, b, _*) if (a.getLabel, b.getLabel) == rule => Some((a,b))
        case List(_*) => None 
      } 

    } 

    /** 
      * @method findBinaryRules 
      * @param rule {(String, String)} 
      * @return {Option[(LinguisticTree, LinguisticTree)]}
      **/
    def findBinaryRules(rules : Set[(String, String)]) : Option[(LinguisticTree, LinguisticTree)] = { 

      rules.foldLeft[Option[(LinguisticTree, LinguisticTree)]](None) { 
        (result, rule) => result match { 
          case Some(_) => result
          case None => tree.findBinaryRule(rule)
        } 
      } 

    } 

  } 

} 
