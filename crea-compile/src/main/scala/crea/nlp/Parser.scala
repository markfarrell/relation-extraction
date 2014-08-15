package crea.nlp

import scalaz._
import Scalaz._

import edu.berkeley.nlp.util.Numberer
import edu.berkeley.nlp.PCFGLA.ParserData
import edu.berkeley.nlp.PCFGLA.Grammar
import edu.berkeley.nlp.PCFGLA.Lexicon
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser

import edu.berkeley.nlp.io.PTBLineLexer

class Parser(grammarPath : String = "eng_sm6.gr") {

  private[this] val threshold : Double = 1.0

  private[this] val viterbi : Boolean = false
  private[this] val substates : Boolean = false
  private[this] val scores : Boolean = false
  private[this] val accurate : Boolean = false
  private[this] val variational : Boolean = false

  private[this] val tokenizer : PTBLineLexer = new PTBLineLexer
  private[this] val pData : ParserData = ParserData.Load(grammarPath)

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

  def apply(sentence : String) : Tree[String] = {
    import Trees._
    parser.getBestConstrainedParse(tokenizer.tokenizeLine(sentence), null, null)
  }

}
