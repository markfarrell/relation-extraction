package edu.berkeley.nlp.syntax

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

  "toDependency" should " produce an Option[Dependency]." in { 
    val tree : LinguisticTree = "((IN after))" 
    val expectation : Option[Environment.Dependency] = Some(Environment.Dependency("after", List()))
    Environment.toDependency(tree) should be (expectation)
  } 

  "toAction" should " produce an Option[Action]." in { 
    val tree : LinguisticTree = "((VP (VBZ walks) (PP (IN because))))"
    val expectation : Option[Environment.Action] = Some(Environment.Action("walks", List(Environment.Dependency("because", List()))))
    Environment.toAction(tree) should be (expectation)
  }

  "toCondition" should " produce an Option[Condition]." in {
     val tree : LinguisticTree = "(ROOT (VP (MD could) (VP (VB walk))))" 
     val expectation : Option[Environment.Condition] = Some(Environment.Condition("could", List(Environment.Action("walk", List()))))

     Environment.toCondition(tree).toString() should be (expectation.toString())
  }

  "toTopic" should " produce an Option[Topic]." in {

     // Case 1: 
     { 
       val tree : LinguisticTree = "((S (NP (DT The) (NN dog) (NN walks.))))"
       val expectation : Option[Environment.Topic] = Some(Environment.Topic("the dog walks.", List()))

       Environment.toTopic(tree).toString() should be (expectation.toString())
     } 

     // Case 2:
     {

       val tree : LinguisticTree = "(S (NP (DT The) (NN dog)) (VP (MD might) (VP (VB eat) (SBAR (IN if) (S (NP (DT the) (NN cat)) (VP (MD can) (VP (VB run.))))))))"

       val expectation : Option[Environment.Topic] = Some(Topic("the dog",
         List(Condition("might", List(Action("eat", List(Dependency("if",
         List(Topic("the cat", List(Condition("can", List(Action("run.",
         List()))))))))))))))

       Environment.toTopic(tree).toString() should be (expectation.toString())

     } 

  }

  "toClause" should " produce an Option[Term]." in { 
     val tree : LinguisticTree = "(ROOT (S (NP (DT The) (NN dog) (NN walks.))))"
     val expectation : Option[Environment.Term] = Some(Environment.Topic("the dog walks.", List()))

     Environment.toClause(tree).toString() should be (expectation.toString())

  }

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
    
     val topic : Environment.Topic= Environment.Topic("the dog", 
       List(Environment.Condition("might", 
         List(Environment.Action("walk", 
           List(Environment.Dependency("if",
             List(Environment.Topic("the cat walks.", List())))))))))

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

     env.insertTopics(List(Environment.toTopic("((S (NP (DT The) (NN dog)) (VP (MD can) (VP (VB walk.)))))").get,
       Environment.toTopic("((S (NP (DT The) (NN dog)) (VP (MD must) (VP (VB run.)))))").get))

     var expectation : String = """<?xml version='1.0' encoding='UTF-8'?>
     <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:viz="http://www.gexf.net/1.2draft/viz" version="1.2">
     <graph defaultedgetype="directed" idtype="string" mode="static"><attributes class="node" mode="static">
     <attribute id="type" title="type" type="string"/></attributes><nodes count="5"><node id="0" label="the dog">
     <attvalues><attvalue for="type" value="Topic"/></attvalues></node>
     <node id="1" label="can"><attvalues><attvalue for="type" value="Condition"/>
     </attvalues></node><node id="2" label="walk."><attvalues><attvalue for="type" value="Action"/>
     </attvalues></node><node id="3" label="must"><attvalues><attvalue for="type" value="Condition"/>
     </attvalues></node><node id="4" label="run."><attvalues><attvalue for="type" value="Action"/></attvalues></node>
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

} 
