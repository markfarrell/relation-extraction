package edu.berkeley.crea.beagle

import edu.berkeley.nlp.util.Numberer
import edu.berkeley.nlp.syntax.Tree
import edu.berkeley.nlp.PCFGLA.ParserData
import edu.berkeley.nlp.PCFGLA.Grammar
import edu.berkeley.nlp.PCFGLA.Lexicon
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser

import edu.berkeley.nlp.io.PTBLineLexer

import org.slf4j.{ Logger, LoggerFactory }

import TreeConversions._

/**
  * Enables the use of the BerkeleyParser for parsing sentences
  * by providing a default configuration.
  * @param grammarFile {String} - Path to load serialized grammar file from.
 **/
class Parser(grammarFile : String = "lib/eng_sm6.gr") {

  private[this] val logger = LoggerFactory.getLogger(classOf[Parser])

  private[this] val threshold : Double = 1.0

  private[this] val viterbi : Boolean = false
  private[this] val substates : Boolean = false
  private[this] val scores : Boolean = false
  private[this] val accurate : Boolean = false
  private[this] val variational : Boolean = false

  private[this] val tokenizer : PTBLineLexer = new PTBLineLexer
  private[this] val pData : ParserData = ParserData.Load(grammarFile)

  private[this] val parser : CoarseToFineMaxRuleParser = {

    assert(pData != null)
    Numberer.setNumberers(pData.getNumbs())

    val p : CoarseToFineMaxRuleParser = new CoarseToFineMaxRuleParser(
      pData.getGrammar, pData.getLexicon, threshold, -1, viterbi,
      substates, scores, accurate, variational, true, true
    )

    p.binarization = pData.getBinarization

    p

  }

 /**
   * Parse an English sentence and returns a parts-of-speech-annotated tree.
   * @param sentence The sentence to be parsed.
  **/
  def apply(sentence : String) : LinguisticTree = {

    import edu.berkeley.nlp.syntax.Trees.PennTreeRenderer

    val tree = parser.getBestConstrainedParse(tokenizer.tokenizeLine(sentence), null, null)

    val rendered = PennTreeRenderer.render(tree)

    logger.debug(s"${sentence}\n${rendered}")

    tree

  }

}
