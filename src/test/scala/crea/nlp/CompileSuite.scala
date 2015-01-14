package crea.nlp

import org.scalatest.FunSuite

import Patterns._
import Terms._

import scalaz._
import Scalaz._

class CompileSuite extends FunSuite {

  private[this] val parse = new Parser

  private[this] def extract(sentence : String) : Option[Stream[Relation]] = {

    RootExpression(parse(sentence))

  }

  test("Noun forms.") {

    val expect = Stream(Relation(Literal("walk"),List(Literal("toronto monkey")))).some

    expect assert_=== extract("The Toronto monkey can walk.")
    expect assert_=== extract("The Toronto monkeys can walk.")
    expect assert_=== extract("The Toronto Monkeys can walk.")

  }

  test("Verb tense and adverbs.") {

    val expect = Stream(Relation(Literal("sleep"), List(Literal("man")))).some

    expect assert_=== extract("The man sleeps.")
    expect assert_=== extract("The man is sleeping.")
    expect assert_=== extract("The man was freely sleeping.")
    expect assert_=== extract("The man freely sleeps.")
    expect assert_=== extract("The man freely has slept.")
    expect assert_=== extract("The man might sleep.")
    expect assert_=== extract("The man might be sleeping.")
    expect assert_=== extract("The man might sleep quietly.")
    expect assert_=== extract("The man might quietly and patiently sleep.")

  }

  test("Phrasal verbs.") {

    val expect = Stream(Relation(Literal("take off"), List(Literal("man")))).some

    expect assert_=== extract("The man takes off.")
    expect assert_=== extract("The man has taken off.")
    expect assert_=== extract("The man might take off.")
    expect assert_=== extract("The man might have taken off.")
    expect assert_=== extract("The man might freely have taken off.")

  }

  test("Prepositional phrases as noun adjectives.") {

    val expect = Stream(Relation(Literal("take"), List(Literal("man type"), Literal("dog")))).some

    expect assert_=== extract("That type of man took the dog.")
    expect assert_=== extract("That type of man has taken the dog.")

  }

  test("Coordinated declarative clauses.") {

    val expect = Stream(Relation(Literal("walk"),List(Literal("man"), Literal("dog"))), Relation(Literal("eat"),List(Literal("man")))).some

    expect assert_=== extract("The man can walk the dog and can eat.")

  }

  test("Single adjunction.") {

    val expect = Stream(Relation(Literal("see"),List(Literal("man"))), Relation(Literal("walk"),List(Literal("man"), Literal("dog")))).some

    expect assert_=== extract("The man if the man can see walked the dog.")
    expect assert_=== extract("The man, if the man can see, can walk the dog.")

  }

  test("Multiple adjunctions.") {

    val expect = Stream(Relation(Literal("see"), List(Literal("man"))), Relation(Literal("walk"), List(Literal("man"))), Relation(Literal("walk"), List(Literal("man"), Literal("dog")))).some

    expect assert_=== extract("The man, if the man can see, and if the man can walk, can walk the dog.")

  }

  test("Inverted adjunction.") {

    val expect = Stream(Relation(Literal("walk"), List(Literal("man"))), Relation(Literal("walk"), List(Literal("dog")))).some

    expect assert_=== extract("If the man can walk the dog can walk.")
    expect assert_=== extract("If the man can walk, the dog can walk.")
    expect assert_=== extract("If the man can walk, then the dog can walk.")

  }

  test("ADJP and VP + PP.") {

    val expect = Stream(Relation(Literal("depend on"), List(Literal("age disease progression"), Literal("health balance")))).some

    expect assert_=== extract("Age disease progression has depended on the critical and crucial health balance.")
    expect assert_=== extract("Age disease progression depends on the critical and crucial health balance.")
    expect assert_=== extract("Age disease progression has depended on the highly critical, crucial health balance.")
    expect assert_=== extract("Age disease progression has depended on the critical and highly crucial health balance.")
    expect assert_=== extract("Age disease progression has depended on the critical, highly crucial health balance.")
    expect assert_=== extract("Age disease progression also depends on the critical, highly crucial health balance.")

  }

  test("VP + PP") {

    val expect = Stream(Relation(Literal("talk about"), List(Literal("man"), Literal("cat")))).some

    expect assert_=== extract("The man can talk about the cat.")

  }

  test("NP + PP and VP + PP.") {

    val expect = Stream(Relation(Literal("depend on"), List(Literal("age progression"), Literal("health balance")))).some

    expect assert_=== extract("Disease progression with aging undoubtedly depends on the critical and crucial health balance.")
    expect assert_=== extract("Disease progression with aging depends undoubtedly on the critical and crucial health balance.")

  }

  test("Also.") {

    val expect = Stream(
      Relation(Literal("worsen"), List(Literal("age disease progression"))),
      Relation(Literal("depend on"), List(Literal("age disease progression"), Literal("health balance")))
    ).some

    expect assert_=== extract("Age disease progression that can worsen also depends on the critical, highly crucial health balance.")

  }

  test("Which.") {

    val expect = Stream(
      Relation(Literal("be"), List(Literal("age disease progression"))),
      Relation(Literal("depend on"), List(Literal("age disease progression"), Literal("health balance")))
    ).some

    expect assert_=== extract("Age disease progression, which is unfortunate, also depends on the critical, highly crucial health balance.")
    expect assert_=== extract("Age disease progression, which is highly unfortunate, also depends on the critical, highly crucial health balance.")

  }

  test("Subject & Object lists.") {

    val expect = Stream(
      Relation(Literal("walk"), List(Literal("type"), Literal("cat"))),
      Relation(Literal("walk"), List(Literal("kind"), Literal("cat"))),
      Relation(Literal("walk"), List(Literal("man class"), Literal("cat"))),
      Relation(Literal("walk"), List(Literal("dog class"), Literal("cat"))),
      Relation(Literal("walk"), List(Literal("type"), Literal("elephant"))),
      Relation(Literal("walk"), List(Literal("kind"), Literal("elephant"))),
      Relation(Literal("walk"), List(Literal("man class"), Literal("elephant"))),
      Relation(Literal("walk"), List(Literal("dog class"), Literal("elephant"))),
      Relation(Literal("walk"), List(Literal("type"), Literal("fox"))),
      Relation(Literal("walk"), List(Literal("kind"), Literal("fox"))),
      Relation(Literal("walk"), List(Literal("man class"), Literal("fox"))),
      Relation(Literal("walk"), List(Literal("dog class"), Literal("fox")))
    ).some

    expect assert_=== extract("That type, kind, and class of man and dog can walk the cat, the elephant and the fox.")

  }

  test("Reporting verb + that clause.") {

    val expect = Stream(
      Relation(Literal("suggest"), List(Literal("study"))),
      Relation(Literal("walk"), List(Literal("man"), Literal("dog")))
    ).some

    expect assert_=== extract("The study suggests the man can walk the dog.")
    expect assert_=== extract("The study suggests that the man can walk the dog.")

  }

  test("Nouns & Conjunction Phrases.") {

    val expect = Stream(
      Relation(Literal("walk"), List(Literal("man"))),
      Relation(Literal("walk"), List(Literal("dog"))),
      Relation(Literal("walk"), List(Literal("cat")))
    ).some

    expect assert_=== extract("The man, as well as the dog and the cat can walk.")
    expect assert_=== extract("The man, the dog and the cat can walk.")
    expect assert_=== extract("The man, the dog as well as the cat can walk.")

  }

  test("Parentheses.") {

    val expect = Stream(
      Relation(Literal("walk"), List(Literal("man"))),
      Relation(Literal("walk"), List(Literal("monkey")))
    ).some

    expect assert_=== extract("The man (monkeys) walked.")

  }

  test("Cardinal Numbers.") {

    val expect = Stream(
      Relation(Literal("participate in"), List(
        Literal("66 woman total"),
        Literal("study")
      ))
    ).some

    expect assert_=== extract("A total of 66 women participated in the study.")

  }

  test("PP + S") {

    val expect = Stream(
      Relation(Literal("create"), List(
        Literal("adult tissue psa expression induction"),
        Literal("condition")
      )),
      Relation(Literal("create"), List(
        Literal("adult tissue psa expression induction"),
        Literal("remodel")
      ))
    ).some

    expect assert_=== extract("The induction of PSA expression in damaged adult tissues could help them rebuild by creating conditions permissive for architectural remodeling.")

  }

  test("Colon") {

    val expect = Stream(
      Relation(Literal("explore in"), List(
        Literal("possibility"),
        Literal("two context")
      )),
      Relation(Literal("explore in"), List(
        Literal("possibility"),
        Literal("axon regeneration")
      )),
      Relation(Literal("explore in"), List(
        Literal("possibility"),
        Literal("precursor recruitment")
      ))
    ).some

    expect assert_=== extract("This possibility has been explored in two contexts: axon regeneration and endogenous neural precursor recruitment.")

  }

}

