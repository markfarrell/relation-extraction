package edu.berkeley.crea.syntax

import Environment.Term
import Environment.Topic
import Environment.Action
import Environment.Condition
import Environment.Dependency

import java.util.Calendar
import java.io.OutputStream
import java.io.StringWriter

import org.scalatest._
import TreeConversions._

import scala.collection.immutable.Stack 

import it.uniroma1.dis.wsngroup.gexf4j.core.GexfWriter
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.StaxGraphWriter
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

class EnvironmentSpec extends FlatSpec with Matchers  {

  "Environments" should " join topics with the same values together when they are inserted." in {

    Environment.randomize = false 

    // Case 1: 
    { 

      val topicA : Environment.Topic= Environment.Topic("the dog", 
        List(Environment.Condition("might", 
          List(Environment.Action("walk", 
            List(Environment.Dependency("if",
              List(Environment.Topic("the cat walks.", List())))))))))

      val topicB : Environment.Topic= Environment.Topic("the dog", 
        List(Environment.Condition("could", 
         List(Environment.Action("sit", List())))))

      val topicC : Environment.Topic = Environment.Topic("the cat walks.", List())

      val topicAB : Environment.Topic = Environment.Topic("the dog", 
        List(Environment.Condition("could", 
          List(Environment.Action("sit", List()))),
            Environment.Condition("might", 
               List(Environment.Action("walk", 
                 List(Environment.Dependency("if",
                   List(Environment.Topic("the cat walks.", List())))))))))

       val env : Environment = new Environment

       env.insertTopics(List(topicB, topicA))

       (env.selectTopics().toString()) should be (List(topicAB, topicC).toString())
     
    }

    // Case 2:
    { 
      val env : Environment = new Environment
      val expected : List[String] = List[String]("the man", "the dog")

      env.insertTopics(List(Environment.Topic("the man", 
        List(Environment.Condition("might", 
          List(Environment.Action("walk", 
            List(Environment.Dependency("if", 
              List(Environment.Topic("the dog", 
                List(Environment.Condition("can", 
                  List(Environment.Action("eat", List.empty)))))))))))))))
      
      env.selectTopics() map { _.value } should be (expected)

    } 

    Environment.randomize = true

  } 

  "toGexf" should " produce a Gexf object." in {

    Environment.randomize = false 

    // Case 1: 
    { 
    
     val topic : Topic= Topic("the dog", List(Condition("might", List(Action("walk", 
       List(Dependency("if", List(Topic("the cat walks.", List())))))))))

     var expectation : String = """<?xml version='1.0' encoding='UTF-8'?>
     <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:viz="http://www.gexf.net/1.2draft/viz" version="1.2">
     <graph defaultedgetype="directed" idtype="string" mode="static"><attributes class="node" mode="static">
     <attribute id="type" title="type" type="string"/></attributes><nodes count="5"><node id="0" label="the dog">
     <attvalues><attvalue for="type" value="Topic"/></attvalues></node><node id="1" label="might"><attvalues>
     <attvalue for="type" value="Condition"/></attvalues></node><node id="2" label="walk">
     <attvalues><attvalue for="type" value="Action"/></attvalues>
     </node><node id="3" label="if"><attvalues><attvalue for="type" value="Dependency"/></attvalues></node>
     <node id="4" label="the cat walks."><attvalues><attvalue for="type" value="Topic"/></attvalues></node>
     </nodes><edges count="4">
     <edge id="0" source="0" target="1" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     <edge id="1" source="1" target="2" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     <edge id="2" source="2" target="3" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     <edge id="3" source="3" target="4" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     </edges></graph></gexf>"""

     expectation = expectation.split("\n").map( _.trim ).mkString("")
 
     // Write Gexf object: OutputStream -> String
     // Compare XML expectation with output
     val gexfWriter : GexfWriter = new StaxGraphWriter()  
     val stringWriter : StringWriter = new StringWriter()
     val gexf : Gexf = Environment.toGexf(List(topic))
     gexfWriter.writeToStream(gexf, stringWriter, "UTF-8")

     stringWriter.toString() should be (expectation)

   }

   // Case 2:
   {

     val env : Environment = new Environment

     env.insertTopics(Environment.parse("((S (NP (DT The) (NN dog)) (VP (MD can) (VP (VB walk.)))))").toList
       ++ Environment.parse("((S (NP (DT The) (NN dog)) (VP (MD must) (VP (VB run.)))))").toList)

     var expectation : String = """<?xml version='1.0' encoding='UTF-8'?>
     <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:viz="http://www.gexf.net/1.2draft/viz" version="1.2">
     <graph defaultedgetype="directed" idtype="string" mode="static"><attributes class="node" mode="static">
     <attribute id="type" title="type" type="string"/></attributes><nodes count="5"><node id="0" label="the dog">
     <attvalues><attvalue for="type" value="Topic"/></attvalues></node>
     <node id="1" label="can"><attvalues><attvalue for="type" value="Condition"/>
     </attvalues></node><node id="2" label="walk"><attvalues><attvalue for="type" value="Action"/>
     </attvalues></node><node id="3" label="must"><attvalues><attvalue for="type" value="Condition"/>
     </attvalues></node><node id="4" label="run"><attvalues><attvalue for="type" value="Action"/></attvalues></node>
     </nodes><edges count="4">
     <edge id="0" source="0" target="1" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     <edge id="2" source="0" target="3" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     <edge id="1" source="1" target="2" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     <edge id="3" source="3" target="4" type="undirected"><viz:color r="0" g="0" b="0"/></edge>
     </edges></graph></gexf>"""

     expectation = expectation.split("\n").map( _.trim ).mkString("")

     val gexfWriter : GexfWriter = new StaxGraphWriter()  
     val stringWriter : StringWriter = new StringWriter()
     val gexf : Gexf = Environment.toGexf(env.selectTopics())
     gexfWriter.writeToStream(gexf, stringWriter, "UTF-8")

     stringWriter.toString() should be (expectation)

   }

   Environment.randomize = true 
      
  }

  "parse" should " produce a stack of Topics." in {

    { 
      val s1 : LinguisticTree = "((S (NP (DT The) (NN dog)) (VP (MD can) (VP (VB walk.)))))"

      val e1 : Stack[Topic] = { 
        Stack[Topic](Topic("the dog", List(Condition("can", List(Action("walk", List()))))))
      } 

      Environment.parse(s1).toString should be (e1.toString)
    } 

    {
      val s2 : LinguisticTree = { 
        "((S (NP (DT The) (NN man)) (VP (MD can) (VP (VB walk) (NP (DT the) (NN dog.))))))"
      }

      val e2 : Stack[Topic] = { 
        Stack[Topic](Topic("the man", List(Condition("can", List(Action("walk", List(Dependency("", List(Topic("the dog", List()))))))))))
      } 

      Environment.parse(s2).toString should be (e2.toString) 
      
    }

    {

      val s3 : LinguisticTree = { 
        "((SQ (MD Will) (NP (DT the) (JJ quick) (NN man)) (VP (VB walk) (NP (DT the) (NN dog?)))))"
      } 

      val e3 : Stack[Topic] = { 
        Stack[Topic](Topic("the quick man", List(Action("walk", List(Dependency("", List(Topic("the dog", List()))))))))
      } 

      Environment.parse(s3).toString should be (e3.toString) 
    }

    { 
      val s4 : LinguisticTree = { 
        "((S (NP (EX There)) (VP (VBZ is) (NP (NP (DT a) (NN man)) (SBAR (WHNP (WDT that)) (S (VP (MD can) (VP (VB walk)))))))))"
      }

      val e4 : Stack[Topic] = { 
        Stack[Topic](Topic("there", List(Action("is", List(Dependency("", List(Topic("a man", List(Condition("can", List(Action("walk", List()))))))))))))
      } 

      Environment.parse(s4).toString should be (e4.toString) 

    }

    {
      val s5 : LinguisticTree = { 
        "((S (NP (DT the) (NN man)) (VP (VBZ has) (VP (VBN hunted) (SBAR (IN until) (S (NP (DT the) (NN dog)) (VP (VBD ate))))))))" 
      }

      val e5 : Stack[Topic] = { 
        Stack[Topic](Topic("the man", List(Action("hunted", List(Dependency("until", List(Topic("the dog", List(Action("ate", List()))))))))))
      } 

      Environment.parse(s5).toString should be (e5.toString)

    } 

  } 

} 
