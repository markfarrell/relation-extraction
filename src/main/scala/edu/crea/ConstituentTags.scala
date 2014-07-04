package edu.crea

import scalaz._
import Scalaz._

import scala.collection.immutable.Stream
import scala.collection.JavaConverters._

package object ConstituentTags {

  sealed trait ConstituentTag

  object S extends ConstituentTag
  object SBAR extends ConstituentTag
  object SBARQ extends ConstituentTag
  object SINV extends ConstituentTag
  object SQ extends ConstituentTag

  object ADJP extends ConstituentTag
  object CONJP extends ConstituentTag
  object FRAG extends ConstituentTag
  object INTJ extends ConstituentTag
  object LST extends ConstituentTag
  object NAC extends ConstituentTag
  object NP extends ConstituentTag
  object NX extends ConstituentTag
  object PP extends ConstituentTag
  object PRN extends ConstituentTag
  object PRT extends ConstituentTag
  object QP extends ConstituentTag
  object RRC extends ConstituentTag
  object UCP extends ConstituentTag
  object VP extends ConstituentTag
  object WHADJP extends ConstituentTag
  object WHAVP extends ConstituentTag
  object WHNP extends ConstituentTag
  object WHPP extends ConstituentTag
  object X extends ConstituentTag

  object _S extends ConstituentTag
  object _SBAR extends ConstituentTag
  object _SBARQ extends ConstituentTag
  object _SINV extends ConstituentTag
  object _SQ extends ConstituentTag

  object _ADJP extends ConstituentTag
  object _CONJP extends ConstituentTag
  object _FRAG extends ConstituentTag
  object _INTJ extends ConstituentTag
  object _LST extends ConstituentTag
  object _NAC extends ConstituentTag
  object _NP extends ConstituentTag
  object _NX extends ConstituentTag
  object _PP extends ConstituentTag
  object _PRN extends ConstituentTag
  object _PRT extends ConstituentTag
  object _QP extends ConstituentTag
  object _RRC extends ConstituentTag
  object _UCP extends ConstituentTag
  object _VP extends ConstituentTag
  object _WHADJP extends ConstituentTag
  object _WHAVP extends ConstituentTag
  object _WHNP extends ConstituentTag
  object _WHPP extends ConstituentTag
  object _X extends ConstituentTag

  object CC extends ConstituentTag
  object CD extends ConstituentTag
  object DT extends ConstituentTag
  object EX extends ConstituentTag
  object FW extends ConstituentTag
  object IN extends ConstituentTag
  object JJ extends ConstituentTag
  object JJR extends ConstituentTag
  object JJS extends ConstituentTag
  object LS extends ConstituentTag
  object MD extends ConstituentTag
  object NN extends ConstituentTag
  object NNS extends ConstituentTag
  object NNP extends ConstituentTag
  object NNPS extends ConstituentTag
  object PDT extends ConstituentTag
  object POS extends ConstituentTag
  object PRP extends ConstituentTag
  object PRP$ extends ConstituentTag
  object RB extends ConstituentTag
  object RBR extends ConstituentTag
  object RBS extends ConstituentTag
  object RP extends ConstituentTag
  object SYM extends ConstituentTag
  object TO extends ConstituentTag
  object UH extends ConstituentTag
  object VB extends ConstituentTag
  object VBD extends ConstituentTag
  object VBG extends ConstituentTag
  object VBN extends ConstituentTag
  object VBP extends ConstituentTag
  object VBZ extends ConstituentTag
  object WDT extends ConstituentTag
  object WP extends ConstituentTag
  object WP$ extends ConstituentTag
  object WRB extends ConstituentTag

  implicit class ConstituentTagger(label : String) {

    def toConstituentTag : Option[ConstituentTag] = label match {
      case "S" => S.some
      case "SBAR" => SBAR.some
      case "SINV" => SINV.some
      case "SQ" => SQ.some
      case "ADJP" => ADJP.some
      case "CONJP" => CONJP.some
      case "FRAG" => FRAG.some
      case "INTJ" => INTJ.some
      case "LST" => LST.some
      case "NAC" => NAC.some
      case "NP" => NP.some
      case "NX" => NX.some
      case "PP" => PP.some
      case "PRN" => PRN.some
      case "PRT" => PRT.some
      case "QP" => QP.some
      case "RRC" => RRC.some
      case "UCP" => UCP.some
      case "VP" => VP.some
      case "WHADJP" => WHADJP.some
      case "WHAVP" => WHAVP.some
      case "WHNP" => WHNP.some
      case "WHPP" => WHPP.some
      case "X" => X.some
      case "@S" => _S.some
      case "@SBAR" => _SBAR.some
      case "@SINV" => _SINV.some
      case "@SQ" => _SQ.some
      case "@ADJP" => _ADJP.some
      case "@CONJP" => _CONJP.some
      case "@FRAG" => _FRAG.some
      case "@INTJ" => _INTJ.some
      case "@LST" => _LST.some
      case "@NAC" => _NAC.some
      case "@NP" => _NP.some
      case "@NX" => _NX.some
      case "@PP" => _PP.some
      case "@PRN" => _PRN.some
      case "@PRT" => _PRT.some
      case "@QP" => _QP.some
      case "@RRC" => _RRC.some
      case "@UCP" => _UCP.some
      case "@VP" => _VP.some
      case "@WHADJP" => _WHADJP.some
      case "@WHAVP" => _WHAVP.some
      case "@WHNP" => _WHNP.some
      case "@WHPP" => _WHPP.some
      case "@X" => _X.some
      case "CC" => CC.some
      case "CD" => CD.some
      case "DT" => DT.some
      case "EX" => EX.some
      case "FW" => FW.some
      case "IN" => IN.some
      case "JJ" => JJ.some
      case "JJR" => JJR.some
      case "JJS" => JJS.some
      case "LS" => LS.some
      case "MD" => MD.some
      case "NN" => NN.some
      case "NNS" => NNS.some
      case "NNP" => NNP.some
      case "NNPS" => NNPS.some
      case "PDT" => PDT.some
      case "POS" => POS.some
      case "PRP" => PRP.some
      case "PRP$" => PRP$.some
      case "RB" => RB.some
      case "RBR" => RBR.some
      case "RP" => RP.some
      case "SYM" => SYM.some
      case "TO" => TO.some
      case "UH" => UH.some
      case "VB" => VB.some
      case "VBD" => VBD.some
      case "VBG" => VBG.some
      case "VBN" => VBN.some
      case "VBP" => VBP.some
      case "VBZ" => VBZ.some
      case "WDT" => WDT.some
      case "WP" => WP.some
      case "WP$" => WP$.some
      case "WRB" => WRB.some
      case _ => none
    }

  }

  implicit class ConstituentTagTree(javaTree : edu.berkeley.nlp.syntax.Tree[String]) {

    def toConstituentTagTree : Tree[String] = {

      val children = javaTree.getChildren.asScala.toStream.map(_.toConstituentTagTree)

      children.size match {
        case 0 => javaTree.getLabel.leaf
        case _ => Tree.node(javaTree.getLabel, children)
      }

    }
  }

}



