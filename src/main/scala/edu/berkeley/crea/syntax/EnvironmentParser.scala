package edu.berkeley.crea.syntax

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.{ 
  List, Set, Stack
}

import TreeConversions._

/** 
 * @class EnvironmentParser
**/ 
class EnvironmentParser {

  import Environment.{ 
    Term, Topic, Condition, Action, Dependency
  } 

  private var topicStack : Stack[Topic] = Stack.empty[Topic]
  private var verbStack : Stack[Term] = Stack.empty[Term] 
  private var dependencyStack : Stack[Dependency] = Stack.empty[Dependency]

  /**
    * @method terminalValue
    * @param tree {LinguisticTree} 
    * @return {String} 
   **/ 
  private def terminalValue(tree : LinguisticTree) : String = {

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
    * @method nextPair
    * @param tree {LinguisticTree} 
    * @return {(LinguisticTree, LinguisticTree)} 
   **/ 
  private def nextPair(tree : LinguisticTree) : (LinguisticTree, LinguisticTree) = { 

    val children = tree.getChildren.asScala

    assert(children.size == 2, "tree has " + children.size + " child node(s), not 2") 

    (children.head, children.last) 
  }

  /**
    * @method existsBinaryRules
    * @param tree {LinguisticTree}
    * @param pairs {Set[(String, String)]} 
    * @return {Boolean} 
   **/
  private def existsBinaryRules(tree : LinguisticTree, pairs : Set[(String, String)]) : Boolean  = {

    val children = tree.getChildren.asScala 

    if(children.size != 2) { 
      false 
    } else {

      pairs exists { 
        rule : (String, String) => existsBinaryRule(tree, rule) 
      } 

    }
  }

  /**
    * @method existsBinaryRule
    * @param tree {LinguisticTree} 
    * @param rule {(String, String)}
    * @return {Boolean} 
   **/
  private def existsBinaryRule(tree : LinguisticTree, rule : (String, String)) : Boolean = { 

    findBinaryRule(tree, rule) match { 
      case Some(_) => true
      case None => false 
    } 

  } 

  /** 
    * @method findBinaryRule
    * @param tree {LinguisticTree} 
    * @param rule {(String, String)} 
    * @return {Option[(LinguisticTree, LinguisticTree)]}  
   **/
  private def findBinaryRule(tree : LinguisticTree, rule : (String, String)) : Option[(LinguisticTree, LinguisticTree)] = { 
    
    tree.getChildren.asScala.toList match {
      case List(a, b, _*) if (a.getLabel, b.getLabel) == rule => Some((a,b))
      case List(_, a, b, _*) if (a.getLabel, b.getLabel) == rule => Some((a,b))
      case List(_*) => None 
    } 

  }

  /**
    * @method parseDependencies
    * @param tree {LinguisticTree} 
    * @return {Stack[Dependency]}
   **/
  private def parseDependencies(tree : LinguisticTree, 
    stack : Stack[Dependency] = Stack.empty[Dependency]) : Stack[Dependency] = {
   
    def hasAPair(tree : LinguisticTree) : Boolean = tree.getChildren.asScala.size == 2 

    def containsTopic(topics : Stack[Topic])(topic : Topic) : Boolean = { 
      topics.find(_.value == topic.value) match { 
        case Some(_) => true
        case None => false
      } 
    }

    def buildDependency(tree : LinguisticTree, value : String = "") : Dependency = {

        val excludeTopics : Stack[Topic] = topicStack
        val topics : List[Topic] = parse(tree).filterNot(containsTopic(excludeTopics)).headOption match { 
          case Some(headTopic) => List[Topic](headTopic)
          case None => List.empty[Topic]
        }

        Dependency(value, topics)
    } 

    tree.getLabel match { 
      case "NP" | "S" => {

        stack.push(buildDependency(tree))

      }
      case "PP" | "SBAR" if hasAPair(tree) => { 

        val (left, right) = nextPair(tree) 
        
        val value : String = terminalValue(left) 

        stack.push(buildDependency(right, value))

      }
    } 

  } 

  /** 
    * @method parseVerb
    * @param tree {LinguisticTree}
    * @return {Stack[Term]}
   **/
  private def parseVerb(tree : LinguisticTree, stack : Stack[Term] = Stack.empty[Term]) : Stack[Term] = {

    def doubleRules : Set[(String, String)] = { 
      Set[(String, String)](
        ("VBZ", "VP"), ("VB", "VP"), 
        ("VBD", "VP"), ("VBP", "VP"),
        ("VBG", "VP"), ("VBN", "VP"),
        ("TO", "VP"))
    } 

    def conditionRules : Set[(String, String)] = { 
      Set[(String, String)](("MD", "VP"))
    }

    def dependencyRules : Set[(String, String)] = { 
      Set[(String, String)](
       // Base form rules 
       ("VB", "PP"), ("VB", "S"), ("VB", "SBAR"), ("VB", "NP"), 
       // Past tense rules 
       ("VBD", "PP"), ("VBD", "S"), ("VBD", "SBAR"), ("VBD", "NP"), 
       // 3rd person singular present rules, e.g. walks  
       ("VBZ", "PP"), ("VBZ", "S"), ("VBZ", "SBAR"), ("VBZ", "NP"), 
       // 3rd person non-singular present rules 
       ("VBP", "PP"), ("VBP", "S"), ("VBP", "SBAR"), ("VBP", "NP"),
       // Gerund verbs, e.g. living 
       ("VBG", "PP"), ("VBG", "S"), ("VBG", "SBAR"), ("VBG", "NP"),
       // Past participle verbs, e.g. lived 
       ("VBN", "PP"), ("VBN", "S"), ("VBN", "SBAR"), ("VBN", "NP"))
    }
    
    tree.getLabel match { 
      case "VP" if existsBinaryRules(tree, conditionRules) => { 

        val (left, right) = nextPair(tree)

        val condition : Condition = {

          val restTerms : List[Term] = parseVerb(right).toList

          val actions : List[Action] = restTerms collect { 
            case a : Action => a 
          } 

          val conditions : List[Condition] = restTerms collect { 
            case c : Condition => c 
          }

          assert(conditions.size == 0, "Conditions should not be nested!")

          Condition(terminalValue(left), actions)

        } 

        stack.push(condition) 

      } 
      case "VP" | "PP" if existsBinaryRules(tree, dependencyRules) => {

        val (left, right) = nextPair(tree) 
      
        val action : Action = { 
          Action(terminalValue(left), parseDependencies(right).toList ++ dependencyStack.toList) 
        }

        dependencyStack = Stack.empty[Dependency]

        stack.push(action) 

      }
      case "VP" if existsBinaryRules(tree, doubleRules) => {

        val (_, right) = nextPair(tree)

        stack ++ parseVerb(right) 

      } 
      case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => { 

        val action : Action = {
          Action(terminalValue(tree), dependencyStack.toList)
        } 

        dependencyStack = Stack.empty[Dependency] 

        stack.push(action) 

      }
      case "NP" if stack.size > 0 && stack.head.isInstanceOf[Action] => { 

        var newStack : Stack[Term] = stack

        val action : Action = { 
          
          val tmp : Action = stack.head.asInstanceOf[Action]
          newStack = newStack.pop
          tmp 
        } 

        val newAction : Action = Action(
          action.value,
          action.dependencies ++ parseDependencies(tree).toList ++ dependencyStack.toList,
          action.color
        )

        dependencyStack = Stack.empty[Dependency]

        newStack.push(newAction)

      }
      case _ => { 

       tree.getChildren.asScala.foldLeft(stack) { 
          (s : Stack[Term], c : LinguisticTree) => { 
            parseVerb(c,s)
          } 
        } 

      } 
    } 
    
  } 

  /** 
    * @method parse 
    * @param tree {LinguisticTree} 
    * @return {Stack[Topic]} 
   **/
  def parse(tree : LinguisticTree) : Stack[Topic] = {

    def thatRules : Set[(String, String)] = { 
      Set[(String, String)](("NP", "SBAR"), ("NP", "PP"))
    }

    def gerundRules : Set[(String, String)] = { 
      Set[(String, String)](("VBG", "NP"))
    } 

    tree.getLabel match {
      case "NP" if !existsBinaryRules(tree, thatRules)  => {

        val topic : Topic = { 
          Topic(terminalValue(tree), verbStack.toList) 
        }

        verbStack = Stack.empty[Term] 

        topicStack = topicStack.push(topic)
        topicStack

      }
      case "VP" if existsBinaryRules(tree, gerundRules) => { 
        
        val (left, right) = nextPair(tree) 

        val topic : Topic = { 
          Topic(terminalValue(right), parseVerb(left).toList ++ verbStack.toList)
        }

        verbStack = Stack.empty[Term]

        topicStack = topicStack.push(topic) 
        topicStack 

      } 
      case "VP" if topicStack.size > 0 => { 

        val topic : Topic = {

          val tmp = topicStack.head
          topicStack = topicStack.pop
          tmp

        } 

        val newTopic : Topic = { 
          Topic(topic.value, topic.abilities ++ parseVerb(tree).toList ++ verbStack.toList)
        }

        verbStack = Stack.empty[Term] 

        topicStack = topicStack.push(newTopic)
        topicStack

      }
      case "VP" => { 
        verbStack = parseVerb(tree) ++ verbStack
        topicStack
      } 
      case "NP" if existsBinaryRules(tree, thatRules) => { 

        val (left, right) = nextPair(tree)

        dependencyStack = { 
          dependencyStack ++ parseDependencies(left) ++ parseDependencies(right)
        }

        topicStack

      } 
      case "SBAR" | "PP" => { 

        dependencyStack = dependencyStack ++ parseDependencies(tree) 
        topicStack 

      } 
      case _ => { // Fold left over the tree's children 
        tree.getChildren.asScala.foldLeft(topicStack) { 
          (s : Stack[Topic], c : LinguisticTree) => parse(c)
        } 
      } 
    } 

  } 
}
