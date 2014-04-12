package edu.berkeley.crea.io

import edu.berkeley.nlp.util.Numberer
import edu.berkeley.nlp.syntax.Tree
import edu.berkeley.nlp.PCFGLA.ParserData
import edu.berkeley.nlp.PCFGLA.Grammar
import edu.berkeley.nlp.PCFGLA.Lexicon 
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser

import edu.berkeley.nlp.io.PTBLineLexer

/** 
  * @class DefaultParser  Enables the use of the BerkeleyParser for parsing sentences 
  * by providing a default configuration.
  * @param grammarFile {String} - Path to load serialized grammar file from. 
 **/ 
class DefaultParser(grammarFile : String) { 

  private val threshold : Double = 1.0 

  private val viterbi : Boolean = false
  private val substates : Boolean = false
  private val scores : Boolean = false 
  private val accurate : Boolean = false
  private val variational : Boolean = false 

  private val tokenizer : PTBLineLexer = new PTBLineLexer
  private val pData : ParserData = ParserData.Load(grammarFile)

  private val parser : CoarseToFineMaxRuleParser = {

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
    * @method getGrammar 
    * @return {Grammar} 
   **/
  def getGrammar() = pData.getGrammar

 /** 
   * @method parse - Parse an English sentence and return a linguistic tree.
   * @param sentence {String} 
   * @return {Tree[String]} 
  **/
  def parse(sentence : String) : Tree[String] = { 
    parser.getBestConstrainedParse(tokenizer.tokenizeLine(sentence) , null, null) 
  } 

} 
