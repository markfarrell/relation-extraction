package edu.berkeley.crea.beagle

import java.util.Properties
import java.io.PrintStream
import java.io.OutputStream

import scala.collection.JavaConverters._

import edu.stanford.nlp.ling.CoreAnnotations.LemmaAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.util.CoreMap

object Lemmatizer {

  val pipeline : StanfordCoreNLP = {

    val errStream : PrintStream = System.err

    System.setErr(new PrintStream(new OutputStream {
      override def write(b : Int) : Unit = Unit
    }))

    val tmp = new StanfordCoreNLP({
      val props : Properties = new Properties
      props.put("annotators", "tokenize, ssplit, pos, lemma")
      props
    })

    System.setErr(errStream)

    tmp
  }

  def lemmatize(str : String) : String = {

    val doc : Annotation = new Annotation(str)

    pipeline.annotate(doc)

    {
      for {
        sentence <- doc.get(classOf[SentencesAnnotation]).asScala
        token <- sentence.get(classOf[TokensAnnotation]).asScala
      } yield token.get(classOf[LemmaAnnotation])
    } mkString("")

  }

}
