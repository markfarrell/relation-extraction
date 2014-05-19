package edu.berkeley.crea.syntax

import scala.collection.JavaConverters._
import scala.collection.Iterator
import scala.collection.immutable.{ 
  List, Set, Stack
}

import org.gephi.graph.api.{ Graph, Node, Edge, GraphFactory, GraphModel } 

import TreeConversions._

/** 
 * @class Compiler - A Compiler front-end that writes to an intermediate in-memory graphstore, 
 * whose contents can then be written to a GEXF file or database back-end. 
 * TODO: Make factory an implicit parameter. 
 **/ 
class Compiler(model : GraphModel) {

  private implicit val factory : GraphFactory = model.factory

  private var topicStack : Stack[Node] = Stack.empty[Node]
  private var verbStack : Stack[Node] = Stack.empty[Node] 
  private var dependencyStack : Stack[Node] = Stack.empty[Node]

  /**
    * @method parseDependencies
    * @param tree {LinguisticTree} 
    * @return {Stack[Node]}
   **/
  private def parseDependencies(tree : LinguisticTree, 
    stack : Stack[Node] = Stack.empty[Node]) : Stack[Node] = {
  
    val excludeTopics : Stack[Node] = topicStack

    val children = tree.getChildren.asScala

    tree.getLabel match { 
      case "NP" | "@NP" | "S" => {

        stack.push(Dependency("", parse(tree), excludeTopics))

      }
      case "PP" | "SBAR" if children.size == 2 => { 

        val (left, right) = (children.head, children.last)

        stack.push(Dependency(left.terminalValue, parse(right), excludeTopics))

      }
    } 

  } 

  /** 
    * @method parseVerb
    * @param tree {LinguisticTree}
    * @return {Stack[Node]}
   **/
  private def parseVerb(tree : LinguisticTree,  
    stack : Stack[Node] = Stack.empty[Node]) : Stack[Node] = {

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

        stack.push(Condition(left.terminalValue, parseVerb(right)))

      } 
      case "VP" | "PP" if tree.existsBinaryRules(dependencyRules) => {

        val (left, right) = tree.findBinaryRules(dependencyRules).get

        val targets = parseDependencies(right) ++ dependencyStack

        dependencyStack = Stack.empty[Node]

        stack.push(Action(left.terminalValue, targets)) 

      }
      case "VP" if tree.existsBinaryRules(doubleRules) => {

        val (_, right) = tree.findBinaryRules(doubleRules).get

        stack ++ parseVerb(right) 

      } 
      case "VB" | "VBD" | "VBZ" | "VBP" | "VBG" | "VBN" => { 

        val targets = dependencyStack

        dependencyStack = Stack.empty[Node] 

        stack.push(Action(tree.terminalValue, targets)) 

      }
      case "NP" if stack.size > 0 => { 

        val action = stack.head

        val targets = parseDependencies(tree) ++ dependencyStack
        dependencyStack = Stack.empty[Node]

        for(node <- targets) { 
          factory.newEdge(action, node) 
        } 

        stack

      }
      case _ => { 

       tree.getChildren.asScala.foldLeft(stack) { 
          (s : Stack[Node], c : LinguisticTree) => { 
            parseVerb(c,s)
          } 
        } 

      } 
    } 
    
  } 

  /** 
    * @method parse 
    * @param tree {LinguisticTree} 
    * @return {Stack[Node]} 
   **/
  def parse(tree : LinguisticTree) : Stack[Node] = {

    def thatRules : Set[(String, String)] = { 
      Set[(String, String)](("NP", "SBAR"), ("NP", "PP"), ("@NP", "SBAR"), ("@NP", "PP"))
    }

    def gerundRules : Set[(String, String)] = { 
      Set[(String, String)](("VBG", "NP"))
    } 

    tree.getLabel match {
      case "NP" | "@NP" if !tree.existsBinaryRules(thatRules) => {

        val targets = verbStack 
        verbStack = Stack.empty[Node] 

        topicStack = topicStack.push(Topic(tree.terminalValue, targets))
        topicStack

      }
      case "VP" if tree.existsBinaryRules(gerundRules) => { 
        
        val (left, right) = tree.findBinaryRules(gerundRules).get

        val targets = parseVerb(left) ++ verbStack  
        verbStack = Stack.empty[Node]

        topicStack = topicStack.push(Topic(right.terminalValue, targets))
        topicStack 

      } 
      case "VP" if topicStack.size > 0 => { 

        val targets = parseVerb(tree) ++ verbStack 
        verbStack = Stack.empty[Node] 

        val topic = topicStack.head

        for(node <- targets) { 
          factory.newEdge(topic, node) 
        } 

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
      case _ => {

        tree.getChildren.asScala.foldLeft(topicStack) { 
          (s : Stack[Node], c : LinguisticTree) => parse(c)
        } 

      } 
    } 

  } 
}


object Topic { 

  def apply(label : String, targets : Stack[Node])(implicit factory : GraphFactory) : Node = {

    val topic = factory.newNode(label)
    topic.setLabel(label) 
    topic.setAttribute("type", "Topic") 

    for(node <- targets) { 
      factory.newEdge(topic, node) 
    } 

    topic 

  } 

} 

object Dependency { 

  def apply(label : String, targets : Stack[Node], excludeTopics : Stack[Node])(implicit factory : GraphFactory) : Node = {

    val lastOption = targets.filterNot(containsTopic(excludeTopics)).lastOption

    val dependency = factory.newNode() 

    dependency.setLabel(label)
    dependency.setAttribute("type", "Dependency")

    for(lastTopic <- lastOption) {
      // TODO: Make the dependency point to the actions of this new topic.
      factory.newEdge(dependency, lastTopic) 
    }

    dependency

  }

  private def containsTopic(topics : Stack[Node])(topic : Node) : Boolean = { 
    topics.find(_.getId == topic.getId) match { 
      case Some(_) => true
      case None => false
    } 
  }

} 

object Action { 

  def apply(label : String, targets : Stack[Node])(implicit factory : GraphFactory) : Node = { 

    val action = factory.newNode() 
    action.setLabel(label) 
    action.setAttribute("type", "Action") 

    for(node <- targets) { 
      factory.newEdge(action, node) 
    }

    action

  } 

}

object Condition { 

  def apply(label : String, targets : Stack[Node])(implicit factory : GraphFactory) : Node = { 

    val condition = factory.newNode()
    condition.setLabel(label) 
    condition.setAttribute("type", "Condition") 

    for(node <- targets) { 
      factory.newEdge(condition, node) 
    } 

    condition 

  }

} 

