package edu.berkeley.nlp.syntax

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
    println(expectation.toString())
    Environment.toDependency(tree) should be (expectation)
  } 

  "toAction" should " produce an Option[Action]." in { 
    val tree : LinguisticTree = "((VP (VBZ walks) (PP (IN because))))"
    val expectation : Option[Environment.Action] = Some(Environment.Action("walks", List(Environment.Dependency("because", List()))))
    Environment.toAction(tree) should be (expectation)

  }

  "toCondition" should " produce an Option[Condition]." in {
     val tree : LinguisticTree = "(VP (MD could) (VP (VB walk)))" 
     val expectation : Option[Environment.Condition] = Some(Environment.Condition("could", List(Environment.Action("walk", List()))))

     Environment.toCondition(tree).toString() should be (expectation.toString())
  }

  "toTopic" should " produce an Option[Topic]." in { 
     val tree : LinguisticTree = "((NP (DT The) (NN dog) (NN walks.)))"
     val expectation : Option[Environment.Topic] = Some(Environment.Topic("The dog walks.", List()))

     Environment.toTopic(tree).toString() should be (expectation.toString())

  }

  "toClause" should " produce an Option[Term]." in { 
     val tree : LinguisticTree = "((NP (DT The) (NN dog) (NN walks.)))"
     val expectation : Option[Environment.Term] = Some(Environment.Topic("The dog walks.", List()))

     Environment.toClause(tree).toString() should be (expectation.toString())

  }

  "Merge" should " combine terms with the same values." in {

    val topicA : Environment.Topic= Environment.Topic("The dog", 
      List(Environment.Condition("might", 
        List(Environment.Action("walk", 
          List(Environment.Dependency("if",
            List(Environment.Topic("the cat walks.", List())))))))))

   val topicB : Environment.Topic= Environment.Topic("The dog", 
     List(Environment.Condition("could", 
       List(Environment.Action("sit", List()))))) 

   val topicAB : Environment.Topic = Environment.Topic("The dog", 
     List(Environment.Condition("could", 
             List(Environment.Action("sit", List()))),
          Environment.Condition("might", 
             List(Environment.Action("walk", 
               List(Environment.Dependency("if",
                 List(Environment.Topic("the cat walks.", List())))))))))

   (Environment.merge(List(topicA, topicB)).toString()) should be (List(topicAB).toString())


  } 

  "toGexf" should " produce a Gexf object." in {
    
    val topic : Environment.Topic= Environment.Topic("The dog", 
      List(Environment.Condition("might", 
        List(Environment.Action("walk", 
          List(Environment.Dependency("if",
            List(Environment.Topic("the cat walks.", List())))))))))

    var expectation : String = """<?xml version='1.0' encoding='UTF-8'?>
    <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:viz="http://www.gexf.net/1.2draft/viz" version="1.2">
    <graph defaultedgetype="undirected" idtype="string" mode="static"><attributes class="node" mode="static">
    <attribute id="type" title="type" type="string"/></attributes><nodes count="5"><node id="The dog" label="The dog">
    <attvalues><attvalue for="type" value="Topic"/></attvalues></node><node id="might" label="might"><attvalues>
    <attvalue for="type" value="Condition"/></attvalues></node><node id="walk" label="walk">
    <attvalues><attvalue for="type" value="Action"/></attvalues>
    </node><node id="if" label="if"><attvalues><attvalue for="type" value="Dependency"/></attvalues></node>
    <node id="the cat walks." label="the cat walks."><attvalues><attvalue for="type" value="Topic"/></attvalues></node>
    </nodes><edges count="4"><edge id="The dog" source="The dog" target="might" type="undirected"/>
    <edge id="might" source="might" target="walk" type="undirected"/><edge id="walk" source="walk" target="if" type="undirected"/>
    <edge id="if" source="if" target="the cat walks." type="undirected"/></edges></graph></gexf>"""

    expectation = expectation.split("\n").map( _.trim ).mkString("")

    // Write Gexf object: OutputStream -> String
    // Compare XML expectation with output
    val gexfWriter : GexfWriter = new StaxGraphWriter()  
    val stringWriter : StringWriter = new StringWriter()
    val gexf : Gexf = Environment.toGexf(List(topic))
    gexfWriter.writeToStream(gexf, stringWriter, "UTF-8")

    stringWriter.toString() should be (expectation)
      
  } 

} 
