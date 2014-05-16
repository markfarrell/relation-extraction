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
    * @method parseDependencies
    * @param tree {LinguisticTree} 
    * @return {Stack[Dependency]}
   **/
  private def parseDependencies(tree : LinguisticTree, 
    stack : Stack[Dependency] = Stack.empty[Dependency]) : Stack[Dependency] = {
   
    def containsTopic(topics : Stack[Topic])(topic : Topic) : Boolean = { 
      topics.find(_.value == topic.value) match { 
        case Some(_) => true
        case None => false
      } 
    }

    def buildDependency(tree : LinguisticTree, value : String = "") : Dependency = {

        val excludeTopics : Stack[Topic] = topicStack
        val topics : List[Topic] = parse(tree).filterNot(containsTopic(excludeTopics)).lastOption match { 
          case Some(headTopic) => List[Topic](headTopic)
          case None => List.empty[Topic]
        }

        // TODO: Make the dependency point to the actions of this new topic. 
        Dependency(value, topics)
    }

    val children = tree.getChildren.asScala

    tree.getLabel match { 
      case "NP" | "@NP" | "S" => {

        stack.push(buildDependency(tree))

      }
      case "PP" | "SBAR" if children.size == 2 => { 

        val (left, right) = (children.head, children.last)
        
        val value : String = left.terminalValue 

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
      case "VP" if tree.existsBinaryRules(conditionRules) => { 

        val (left, right) = tree.findBinaryRules(conditionRules).get

        val condition : Condition = {

          val restTerms : List[Term] = parseVerb(right).toList

          val actions : List[Action] = restTerms collect { 
            case a : Action => a 
          } 

          val conditions : List[Condition] = restTerms collect { 
            case c : Condition => c 
          }

          assert(conditions.size == 0, "Conditions should not be nested!")

          Condition(left.terminalValue, actions)

        } 

        stack.push(condition) 

      } 
      case "VP" | "PP" if tree.existsBinaryRules(dependencyRules) => {

        val (left, right) = tree.findBinaryRules(dependencyRules).get
      
        val action : Action = { 
          Action(left.terminalValue, parseDependencies(right).toList ++ dependencyStack.toList) 
        }

        dependencyStack = Stack.empty[Dependency]

        stack.push(action) 

      }
      case "VP" if tree.existsBinaryRules(doubleRules) => {

        val (_, right) = tree.findBinaryRules(doubleRules).get

        stack ++ parseVerb(right) 

      } 
      case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => { 

        val action : Action = {
          Action(tree.terminalValue, dependencyStack.toList)
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
          action.dependencies ++ parseDependencies(tree).toList ++ dependencyStack.toList
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
      Set[(String, String)](("NP", "SBAR"), ("NP", "PP"), ("@NP", "SBAR"), ("@NP", "PP"))
    }

    def gerundRules : Set[(String, String)] = { 
      Set[(String, String)](("VBG", "NP"))
    } 

    tree.getLabel match {
      case "NP" | "@NP" if !tree.existsBinaryRules(thatRules)  => {

        val topic : Topic = { 
          Topic(tree.terminalValue, verbStack.toList) 
        }

        verbStack = Stack.empty[Term] 

        topicStack = topicStack.push(topic)
        topicStack

      }
      case "VP" if tree.existsBinaryRules(gerundRules) => { 
        
        val (left, right) = tree.findBinaryRules(gerundRules).get

        val topic : Topic = { 
          Topic(right.terminalValue, parseVerb(left).toList ++ verbStack.toList)
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
      case "NP" | "@NP" if tree.existsBinaryRules(thatRules) => { 

        val (left, right) = tree.findBinaryRules(thatRules).get

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
