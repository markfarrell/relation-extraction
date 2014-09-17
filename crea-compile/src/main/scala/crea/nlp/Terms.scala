package crea.nlp

import scalaz._
import Scalaz._

package object Terms {

  sealed trait Term

  final case class Literal(id : String) extends Term

  final case class Relation(literal : Literal = Monoid[Literal].zero, args : List[Literal] = Monoid[List[Literal]].zero) extends Term

  implicit val equalLiteral : Equal[Literal] = Equal.equal(_.id === _.id)

  implicit val equalRelation : Equal[Relation] = Equal.equal {
    (x, y) => (x.literal, x.args) === (y.literal, y.args)
  }

  implicit val monoidLiteral : Monoid[Literal] = new Monoid[Literal] {

    def zero : Literal = Literal(Monoid[String].zero)

    def append(a1 : Literal, a2: => Literal) : Literal = Literal(List(a1.id, a2.id).distinct.mkString(" ").trim)

  }

  implicit val monoidRelation : Monoid[Relation] = new Monoid[Relation] {

    def zero : Relation = Relation(Monoid[Literal].zero, Monoid[List[Literal]].zero)

    def append(a1 : Relation, a2 : => Relation) : Relation = {
      Relation(a1.literal |+| a2.literal, (a1.args |+| a2.args).filterNot(_ === Monoid[Literal].zero).distinct)
    }

  }

  implicit val showLiteral : Show[Literal] = new Show[Literal] {

    override def shows(literal : Literal) : String = s"<literal:${literal.id}>"

  }

  implicit val showRelation : Show[Relation] = new Show[Relation] {

    override def shows(relation : Relation) : String = s"""<relation:${relation.literal.id}(${relation.args.map(_.shows).mkString(", ")})>"""

  }

  implicit val showRelationStream : Show[Stream[Relation]] = new Show[Stream[Relation]] {

    override def shows(relations : Stream[Relation]) : String = relations.iterator.map(_.shows).mkString("\n")

  }

}
