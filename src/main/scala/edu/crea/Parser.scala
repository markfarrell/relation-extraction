package edu.crea

import edu.berkeley.nlp.util.Numberer
import edu.berkeley.nlp.syntax.Tree
import edu.berkeley.nlp.PCFGLA.ParserData
import edu.berkeley.nlp.PCFGLA.Grammar
import edu.berkeley.nlp.PCFGLA.Lexicon
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser

import edu.berkeley.nlp.io.PTBLineLexer

import TreeConversions._

/**
  * Enables the use of the BerkeleyParser for parsing sentences
  * by providing a default configuration.
  * @param grammarFile {String} - Path to load serialized grammar file from.
 **/
class Parser(grammarFile : String = "eng_sm6.gr") {

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
  def apply(sentence : String) : Tree[String] = {
    parser.getBestConstrainedParse(tokenizer.tokenizeLine(sentence), null, null)
  }

}
