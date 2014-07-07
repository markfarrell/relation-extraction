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

  object AtS extends ConstituentTag
  object AtSBAR extends ConstituentTag
  object AtSBARQ extends ConstituentTag
  object AtSINV extends ConstituentTag
  object AtSQ extends ConstituentTag

  object AtADJP extends ConstituentTag
  object AtCONJP extends ConstituentTag
  object AtFRAG extends ConstituentTag
  object AtINTJ extends ConstituentTag
  object AtLST extends ConstituentTag
  object AtNAC extends ConstituentTag
  object AtNP extends ConstituentTag
  object AtNX extends ConstituentTag
  object AtPP extends ConstituentTag
  object AtPRN extends ConstituentTag
  object AtPRT extends ConstituentTag
  object AtQP extends ConstituentTag
  object AtRRC extends ConstituentTag
  object AtUCP extends ConstituentTag
  object AtVP extends ConstituentTag
  object AtWHADJP extends ConstituentTag
  object AtWHAVP extends ConstituentTag
  object AtWHNP extends ConstituentTag
  object AtWHPP extends ConstituentTag
  object AtX extends ConstituentTag

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

  case class Terminal(label : String) extends ConstituentTag

  implicit class ConstituentTagger(label : String) {

    def toConstituentTag : ConstituentTag = label match {
      case "S" => S
      case "SBARQ" => SBARQ
      case "SBAR" => SBAR
      case "SINV" => SINV
      case "SQ" => SQ
      case "ADJP" => ADJP
      case "CONJP" => CONJP
      case "FRAG" => FRAG
      case "INTJ" => INTJ
      case "LST" => LST
      case "NAC" => NAC
      case "NP" => NP
      case "NX" => NX
      case "PP" => PP
      case "PRN" => PRN
      case "PRT" => PRT
      case "QP" => QP
      case "RRC" => RRC
      case "UCP" => UCP
      case "VP" => VP
      case "WHADJP" => WHADJP
      case "WHAVP" => WHAVP
      case "WHNP" => WHNP
      case "WHPP" => WHPP
      case "X" => X
      case "@S" => AtS
      case "@SBARQ" => AtSBARQ
      case "@SBAR" => AtSBAR
      case "@SINV" => AtSINV
      case "@SQ" => AtSQ
      case "@ADJP" => AtADJP
      case "@CONJP" => AtCONJP
      case "@FRAG" => AtFRAG
      case "@INTJ" => AtINTJ
      case "@LST" => AtLST
      case "@NAC" => AtNAC
      case "@NP" => AtNP
      case "@NX" => AtNX
      case "@PP" => AtPP
      case "@PRN" => AtPRN
      case "@PRT" => AtPRT
      case "@QP" => AtQP
      case "@RRC" => AtRRC
      case "@UCP" => AtUCP
      case "@VP" => AtVP
      case "@WHADJP" => AtWHADJP
      case "@WHAVP" => AtWHAVP
      case "@WHNP" => AtWHNP
      case "@WHPP" => AtWHPP
      case "@X" => AtX
      case "CC" => CC
      case "CD" => CD
      case "DT" => DT
      case "EX" => EX
      case "FW" => FW
      case "IN" => IN
      case "JJ" => JJ
      case "JJR" => JJR
      case "JJS" => JJS
      case "LS" => LS
      case "MD" => MD
      case "NN" => NN
      case "NNS" => NNS
      case "NNP" => NNP
      case "NNPS" => NNPS
      case "PDT" => PDT
      case "POS" => POS
      case "PRP" => PRP
      case "PRP$" => PRP$
      case "RB" => RB
      case "RBR" => RBR
      case "RBS" => RBS
      case "RP" => RP
      case "SYM" => SYM
      case "TO" => TO
      case "UH" => UH
      case "VB" => VB
      case "VBD" => VBD
      case "VBG" => VBG
      case "VBN" => VBN
      case "VBP" => VBP
      case "VBZ" => VBZ
      case "WDT" => WDT
      case "WP" => WP
      case "WP$" => WP$
      case "WRB" => WRB
      case _ => Terminal(label)
    }

  }

  implicit val ConstituentTagShow : Show[ConstituentTag] = new Show[ConstituentTag] {

    override def show(a : ConstituentTag) = a match {
      case S => "S"
      case SBARQ => "SBARQ"
      case SBAR => "SBAR"
      case SINV => "SINV"
      case SQ => "SQ"
      case ADJP => "ADJP"
      case CONJP => "CONJP"
      case FRAG => "FRAG"
      case INTJ => "INTJ"
      case LST => "LST"
      case NAC => "NAC"
      case NP => "NP"
      case NX => "NX"
      case PP => "PP"
      case PRN => "PRN"
      case PRT => "PRT"
      case QP => "QP"
      case RRC => "RRC"
      case UCP => "UCP"
      case VP => "VP"
      case WHADJP => "WHADJP"
      case WHAVP => "WHAVP"
      case WHNP => "WHNP"
      case WHPP => "WHPP"
      case X => "X"
      case AtS => "@S"
      case AtSBARQ => "@SBARQ"
      case AtSBAR => "@SBAR"
      case AtSINV => "@SINV"
      case AtSQ => "@SQ"
      case AtADJP => "@ADJP"
      case AtCONJP => "@CONJP"
      case AtFRAG => "@FRAG"
      case AtINTJ => "@INTJ"
      case AtLST => "@LST"
      case AtNAC => "@NAC"
      case AtNP => "@NP"
      case AtNX => "@NX"
      case AtPP => "@PP"
      case AtPRN => "@PRN"
      case AtPRT => "@PRT"
      case AtQP => "@QP"
      case AtRRC => "@RRC"
      case AtUCP => "@UCP"
      case AtVP => "@VP"
      case AtWHADJP => "@WHADJP"
      case AtWHAVP => "@WHAVP"
      case AtWHNP => "@WHNP"
      case AtWHPP => "@WHPP"
      case AtX => "@X"
      case CC => "CC"
      case CD => "CD"
      case DT => "DT"
      case EX => "EX"
      case FW => "FW"
      case IN => "IN"
      case JJ => "JJ"
      case JJR => "JJR"
      case JJS => "JJS"
      case LS => "LS"
      case MD => "MD"
      case NN => "NN"
      case NNS => "NNS"
      case NNP => "NNP"
      case NNPS => "NNPS"
      case PDT => "PDT"
      case POS => "POS"
      case PRP => "PRP"
      case PRP$ => "PRP$"
      case RB => "RB"
      case RBR => "RBR"
      case RBS => "RBS"
      case RP => "RP"
      case SYM => "SYM"
      case TO => "TO"
      case UH => "UH"
      case VB => "VB"
      case VBD => "VBD"
      case VBG => "VBG"
      case VBN => "VBN"
      case VBP => "VBP"
      case VBZ => "VBZ"
      case WDT => "WDT"
      case WP => "WP"
      case WP$ => "WP$"
      case WRB => "WRB"
      case Terminal(label) => label
    }
  }

  object ConstituentTagTree {

    def apply(javaTree : edu.berkeley.nlp.syntax.Tree[String]) : Tree[ConstituentTag] = {

      val children = javaTree.getChildren.asScala.toStream.map(ConstituentTagTree.apply)

      children.size match {
        case 0 => javaTree.getLabel.toConstituentTag.leaf
        case _ => Tree.node(javaTree.getLabel.toConstituentTag, children)
      }

    }

  }

}



