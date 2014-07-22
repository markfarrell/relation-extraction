package edu.crea

import org.scalatest.FunSuite

import Patterns._
import Terms._

import scalaz._
import Scalaz._

object Compile {

  private[this] val parse = new Parser

  def apply(sentence : String) : Option[Stream[Compound]] = RootExpression(parse(sentence))

}

class CompileSuite extends FunSuite {

  test("Noun forms.") {

    val expect = Stream(Compound(Atom("walk"),Stream(Atom("toronto monkey")))).some

    expect assert_=== Compile("The Toronto monkey can walk.")
    expect assert_=== Compile("The Toronto monkeys can walk.")
    expect assert_=== Compile("The Toronto Monkeys can walk.")

  }

  test("Verb tense and adverbs.") {

    val expect = Stream(Compound(Atom("sleep"), Stream(Atom("man")))).some

    expect assert_=== Compile("The man sleeps.")
    expect assert_=== Compile("The man is sleeping.")
    expect assert_=== Compile("The man was freely sleeping.")
    expect assert_=== Compile("The man freely sleeps.")
    expect assert_=== Compile("The man freely has slept.")
    expect assert_=== Compile("The man might sleep.")
    expect assert_=== Compile("The man might be sleeping.")
    expect assert_=== Compile("The man might sleep quietly.")
    expect assert_=== Compile("The man might quietly and patiently sleep.")

  }

  test("Phrasal verbs.") {

    val expect = Stream(Compound(Atom("take off"), Stream(Atom("man")))).some

    expect assert_=== Compile("The man takes off.")
    expect assert_=== Compile("The man has taken off.")
    expect assert_=== Compile("The man might take off.")
    expect assert_=== Compile("The man might have taken off.")
    expect assert_=== Compile("The man might freely have taken off.")

  }

  test("Prepositional phrases as noun adjectives.") {

    val expect = Stream(Compound(Atom("take"),Stream(Atom("man type"), Atom("dog")))).some

    expect assert_=== Compile("That type of man took the dog.")
    expect assert_=== Compile("That type of man has taken the dog.")

  }

  test("Coordinated declarative clauses.") {

    val expect = Stream(Compound(Atom("walk"),Stream(Atom("man"), Atom("dog"))), Compound(Atom("eat"),Stream(Atom("man")))).some

    expect assert_=== Compile("The man can walk the dog and can eat.")

  }

  test("Single adjunction.") {

    val expect = Stream(Compound(Atom("see"),Stream(Atom("man"))), Compound(Atom("walk"),Stream(Atom("man"), Atom("dog")))).some

    expect assert_=== Compile("The man if the man can see walked the dog.")
    expect assert_=== Compile("The man, if the man can see, can walk the dog.")

  }

  test("Multiple adjunctions.") {

    val expect = Stream(Compound(Atom("see"), Stream(Atom("man"))), Compound(Atom("walk"), Stream(Atom("man"))), Compound(Atom("walk"), Stream(Atom("man"), Atom("dog")))).some

    expect assert_=== Compile("The man, if the man can see, and if the man can walk, can walk the dog.")

  }

  test("Inverted adjunction.") {

    val expect = Stream(Compound(Atom("walk"), Stream(Atom("man"))), Compound(Atom("walk"), Stream(Atom("dog")))).some

    expect assert_=== Compile("If the man can walk the dog can walk.")
    expect assert_=== Compile("If the man can walk, the dog can walk.")
    expect assert_=== Compile("If the man can walk, then the dog can walk.")

  }

  test("ADJP and VP + PP.") {

    val expect = Stream(Compound(Atom("depend"), Stream(Atom("age disease progression"), Atom("health balance")))).some

    expect assert_=== Compile("Age disease progression has depended on the critical and crucial health balance.")
    expect assert_=== Compile("Age disease progression depends on the critical and crucial health balance.")
    expect assert_=== Compile("Age disease progression has depended on the highly critical, crucial health balance.")
    expect assert_=== Compile("Age disease progression has depended on the critical and highly crucial health balance.")
    expect assert_=== Compile("Age disease progression has depended on the critical, highly crucial health balance.")
    expect assert_=== Compile("Age disease progression also depends on the critical, highly crucial health balance.")

  }

  test("VP + PP") {

    val expect = Stream(Compound(Atom("talk"), Stream(Atom("man"), Atom("cat")))).some

    expect assert_=== Compile("The man can talk about the cat.")

  }

  test("NP + PP and VP + PP.") {

    val expect = Stream(Compound(Atom("depend"), Stream(Atom("age progression"), Atom("health balance")))).some

    expect assert_=== Compile("Disease progression with aging undoubtedly depends on the critical and crucial health balance.")
    expect assert_=== Compile("Disease progression with aging depends undoubtedly on the critical and crucial health balance.")

  }

  test("Also.") {

    val expect = Stream(
      Compound(Atom("worsen"), Stream(Atom("age disease progression"))),
      Compound(Atom("depend"), Stream(Atom("age disease progression"), Atom("health balance")))
    ).some

    expect assert_=== Compile("Age disease progression that can worsen also depends on the critical, highly crucial health balance.")

  }

  test("Which.") {

    val expect = Stream(
      Compound(Atom("be"), Stream(Atom("age disease progression"))),
      Compound(Atom("depend"), Stream(Atom("age disease progression"), Atom("health balance")))
    ).some

    expect assert_=== Compile("Age disease progression, which is unfortunate, also depends on the critical, highly crucial health balance.")
    expect assert_=== Compile("Age disease progression, which is highly unfortunate, also depends on the critical, highly crucial health balance.")

  }

}

