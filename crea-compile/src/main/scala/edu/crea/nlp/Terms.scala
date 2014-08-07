package edu.crea.nlp

import scalaz._
import Scalaz._

package object Terms {

  sealed trait Term

  case class Atom(id : String) extends Term

  case class Compound(atom : Atom = Monoid[Atom].zero, args : List[Atom] = Monoid[List[Atom]].zero) extends Term

  implicit val equalAtom : Equal[Atom] = Equal.equal(_.id === _.id)

  implicit val equalCompound : Equal[Compound] = Equal.equal {
    (x, y) => (x.atom, x.args) === (y.atom, y.args)
  }

  implicit val monoidAtom : Monoid[Atom] = new Monoid[Atom] {

    def zero : Atom = Atom(Monoid[String].zero)

    def append(a1 : Atom, a2: => Atom) : Atom = Atom(List(a1.id, a2.id).distinct.mkString(" ").trim)

  }

  implicit val monoidCompound : Monoid[Compound] = new Monoid[Compound] {

    def zero : Compound = Compound(Monoid[Atom].zero, Monoid[List[Atom]].zero)

    def append(a1 : Compound, a2 : => Compound) : Compound = {
      Compound(a1.atom |+| a2.atom, (a1.args |+| a2.args).filterNot(_ === Monoid[Atom].zero).distinct)
    }

  }

  implicit val showAtom : Show[Atom] = new Show[Atom] {

    override def shows(atom : Atom) : String = s"<atom:${atom.id}>"

  }

  implicit val showCompound : Show[Compound] = new Show[Compound] {

    override def shows(compound : Compound) : String = s"""<compound:${compound.atom.id}(${compound.args.map(_.shows).mkString(", ")})>"""

  }

  implicit val showCompoundStream : Show[Stream[Compound]] = new Show[Stream[Compound]] {

    override def shows(compounds : Stream[Compound]) : String = compounds.iterator.map(_.shows).mkString("\n")

  }

}
