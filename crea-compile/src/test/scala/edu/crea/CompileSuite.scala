package edu.crea.nlp

import org.scalatest.FunSuite

import Patterns._
import Terms._

import scalaz._
import Scalaz._

class CompileSuite extends FunSuite {

  private[this] val parse = new Parser

  private[this] def compile(sentence : String) : Option[Stream[Compound]] = RootExpression(parse(sentence))

  test("Noun forms.") {

    val expect = Stream(Compound(Atom("walk"),List(Atom("toronto monkey")))).some

    expect assert_=== compile("The Toronto monkey can walk.")
    expect assert_=== compile("The Toronto monkeys can walk.")
    expect assert_=== compile("The Toronto Monkeys can walk.")

  }

  test("Verb tense and adverbs.") {

    val expect = Stream(Compound(Atom("sleep"), List(Atom("man")))).some

    expect assert_=== compile("The man sleeps.")
    expect assert_=== compile("The man is sleeping.")
    expect assert_=== compile("The man was freely sleeping.")
    expect assert_=== compile("The man freely sleeps.")
    expect assert_=== compile("The man freely has slept.")
    expect assert_=== compile("The man might sleep.")
    expect assert_=== compile("The man might be sleeping.")
    expect assert_=== compile("The man might sleep quietly.")
    expect assert_=== compile("The man might quietly and patiently sleep.")

  }

  test("Phrasal verbs.") {

    val expect = Stream(Compound(Atom("take off"), List(Atom("man")))).some

    expect assert_=== compile("The man takes off.")
    expect assert_=== compile("The man has taken off.")
    expect assert_=== compile("The man might take off.")
    expect assert_=== compile("The man might have taken off.")
    expect assert_=== compile("The man might freely have taken off.")

  }

  test("Prepositional phrases as noun adjectives.") {

    val expect = Stream(Compound(Atom("take"),List(Atom("man type"), Atom("dog")))).some

    expect assert_=== compile("That type of man took the dog.")
    expect assert_=== compile("That type of man has taken the dog.")

  }

  test("Coordinated declarative clauses.") {

    val expect = Stream(Compound(Atom("walk"),List(Atom("man"), Atom("dog"))), Compound(Atom("eat"),List(Atom("man")))).some

    expect assert_=== compile("The man can walk the dog and can eat.")

  }

  test("Single adjunction.") {

    val expect = Stream(Compound(Atom("see"),List(Atom("man"))), Compound(Atom("walk"),List(Atom("man"), Atom("dog")))).some

    expect assert_=== compile("The man if the man can see walked the dog.")
    expect assert_=== compile("The man, if the man can see, can walk the dog.")

  }

  test("Multiple adjunctions.") {

    val expect = Stream(Compound(Atom("see"), List(Atom("man"))), Compound(Atom("walk"), List(Atom("man"))), Compound(Atom("walk"), List(Atom("man"), Atom("dog")))).some

    expect assert_=== compile("The man, if the man can see, and if the man can walk, can walk the dog.")

  }

  test("Inverted adjunction.") {

    val expect = Stream(Compound(Atom("walk"), List(Atom("man"))), Compound(Atom("walk"), List(Atom("dog")))).some

    expect assert_=== compile("If the man can walk the dog can walk.")
    expect assert_=== compile("If the man can walk, the dog can walk.")
    expect assert_=== compile("If the man can walk, then the dog can walk.")

  }

  test("ADJP and VP + PP.") {

    val expect = Stream(Compound(Atom("depend"), List(Atom("age disease progression"), Atom("health balance")))).some

    expect assert_=== compile("Age disease progression has depended on the critical and crucial health balance.")
    expect assert_=== compile("Age disease progression depends on the critical and crucial health balance.")
    expect assert_=== compile("Age disease progression has depended on the highly critical, crucial health balance.")
    expect assert_=== compile("Age disease progression has depended on the critical and highly crucial health balance.")
    expect assert_=== compile("Age disease progression has depended on the critical, highly crucial health balance.")
    expect assert_=== compile("Age disease progression also depends on the critical, highly crucial health balance.")

  }

  test("VP + PP") {

    val expect = Stream(Compound(Atom("talk"), List(Atom("man"), Atom("cat")))).some

    expect assert_=== compile("The man can talk about the cat.")

  }

  test("NP + PP and VP + PP.") {

    val expect = Stream(Compound(Atom("depend"), List(Atom("age progression"), Atom("health balance")))).some

    expect assert_=== compile("Disease progression with aging undoubtedly depends on the critical and crucial health balance.")
    expect assert_=== compile("Disease progression with aging depends undoubtedly on the critical and crucial health balance.")

  }

  test("Also.") {

    val expect = Stream(
      Compound(Atom("worsen"), List(Atom("age disease progression"))),
      Compound(Atom("depend"), List(Atom("age disease progression"), Atom("health balance")))
    ).some

    expect assert_=== compile("Age disease progression that can worsen also depends on the critical, highly crucial health balance.")

  }

  test("Which.") {

    val expect = Stream(
      Compound(Atom("be"), List(Atom("age disease progression"))),
      Compound(Atom("depend"), List(Atom("age disease progression"), Atom("health balance")))
    ).some

    expect assert_=== compile("Age disease progression, which is unfortunate, also depends on the critical, highly crucial health balance.")
    expect assert_=== compile("Age disease progression, which is highly unfortunate, also depends on the critical, highly crucial health balance.")

  }

  test("Subject & Object lists.") {

    val expect = Stream(
      Compound(Atom("walk"), List(Atom("type"), Atom("cat"))),
      Compound(Atom("walk"), List(Atom("kind"), Atom("cat"))),
      Compound(Atom("walk"), List(Atom("man class"), Atom("cat"))),
      Compound(Atom("walk"), List(Atom("dog class"), Atom("cat"))),
      Compound(Atom("walk"), List(Atom("type"), Atom("elephant"))),
      Compound(Atom("walk"), List(Atom("kind"), Atom("elephant"))),
      Compound(Atom("walk"), List(Atom("man class"), Atom("elephant"))),
      Compound(Atom("walk"), List(Atom("dog class"), Atom("elephant"))),
      Compound(Atom("walk"), List(Atom("type"), Atom("fox"))),
      Compound(Atom("walk"), List(Atom("kind"), Atom("fox"))),
      Compound(Atom("walk"), List(Atom("man class"), Atom("fox"))),
      Compound(Atom("walk"), List(Atom("dog class"), Atom("fox")))
    ).some

    expect assert_=== compile("That type, kind, and class of man and dog can walk the cat, the elephant and the fox.")

  }

}

