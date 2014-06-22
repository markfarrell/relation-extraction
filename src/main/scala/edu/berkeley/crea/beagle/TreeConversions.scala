package edu.berkeley.crea.beagle

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

  implicit def StringToTree(str : String) : LinguisticTree = Trees.PennTreeReader.parseEasy(str, false)

  implicit class TreeEnhancer(tree : LinguisticTree) {

    // TODO: Load from a file.
    def blacklist = List("we", "they", "it", "way", "much", "other",
      "many", "most", "only", "whereas", "such", "more", "few",
      "less", "one", "different", "various", "several", "than",
      "certain", "``", "\"", "as", "two", "three", "four", "five", "six",
      "seven", "eight", "nine", "ten", "by", "know", "at", "themselves",
      "itself", "''", "whose", "about", "i", "ii", "iii", "iv", "v", "vi",
      "vii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "to", "at", "first",
      "second", "third", "fourth", "fifth", "six", "seventh", "eighth", "ninth",
      "tenth", "there", "example", "ways", "over", "between", "today", "yesterday",
      "tomorrow", "'s", "new", "old", "among", "fewer", "where", "easily", "outermost",
      "innermost", "remarkable", "above", "below", "~")

    /**
      * Collects the labels of the terminal nodes in the tree. Lowercases each label.
      * Ignores possessive pronouns, personal pronouns, punctuation, determiners, adverbs,
      * comparative adjectives  and superlative adjectives.
     **/
    def terminalValue : String = {

      def terminalLabels(tree : LinguisticTree) : String = {
        tree.getTerminals.asScala
          .map { _.getLabel }
          .map(Lemmatizer.lemmatize)
          .filterNot(blacklist.contains)
          .mkString("")
      }

      val str = tree.iterator.asScala.toList.filter {
        _.isPreTerminal
      }.map(terminalLabels).filterNot(_ == "").mkString(" ")

      str.toLowerCase

    }

    def existsBinaryRules(pairs : Set[(String, String)]) : Boolean  = {

      pairs exists {
        rule : (String, String) => tree.existsBinaryRule(rule)
      }

    }

    def existsBinaryRule(rule : (String, String)) : Boolean = {

      tree.findBinaryRule(rule) match {
        case Some(_) => true
        case None => false
      }

    }

    def findBinaryRule(rule : (String, String)) : Option[(LinguisticTree, LinguisticTree)] = {

      tree.getChildren.asScala.toList match {
        case List(a, b, _*) if (a.getLabel, b.getLabel) == rule => Some((a,b))
        case List(_*) => None
      }

    }

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
