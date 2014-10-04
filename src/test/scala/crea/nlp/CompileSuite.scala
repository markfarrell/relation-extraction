package crea.nlp

import org.scalatest.FunSuite

import Patterns._
import Terms._

import scalaz._
import Scalaz._

class CompileSuite extends FunSuite {

  private[this] val parse = new Parser

  private[this] def compile(sentence : String) : Option[Stream[Relation]] = RootExpression(parse(sentence))

  test("Noun forms.") {

    val expect = Stream(Relation(Literal("walk"),List(Literal("toronto monkey")))).some

    expect assert_=== compile("The Toronto monkey can walk.")
    expect assert_=== compile("The Toronto monkeys can walk.")
    expect assert_=== compile("The Toronto Monkeys can walk.")

  }

  test("Verb tense and adverbs.") {

    val expect = Stream(Relation(Literal("sleep"), List(Literal("man")))).some

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

    val expect = Stream(Relation(Literal("take off"), List(Literal("man")))).some

    expect assert_=== compile("The man takes off.")
    expect assert_=== compile("The man has taken off.")
    expect assert_=== compile("The man might take off.")
    expect assert_=== compile("The man might have taken off.")
    expect assert_=== compile("The man might freely have taken off.")

  }

  test("Prepositional phrases as noun adjectives.") {

    val expect = Stream(Relation(Literal("take"),List(Literal("man type"), Literal("dog")))).some

    expect assert_=== compile("That type of man took the dog.")
    expect assert_=== compile("That type of man has taken the dog.")

  }

  test("Coordinated declarative clauses.") {

    val expect = Stream(Relation(Literal("walk"),List(Literal("man"), Literal("dog"))), Relation(Literal("eat"),List(Literal("man")))).some

    expect assert_=== compile("The man can walk the dog and can eat.")

  }

  test("Single adjunction.") {

    val expect = Stream(Relation(Literal("see"),List(Literal("man"))), Relation(Literal("walk"),List(Literal("man"), Literal("dog")))).some

    expect assert_=== compile("The man if the man can see walked the dog.")
    expect assert_=== compile("The man, if the man can see, can walk the dog.")

  }

  test("Multiple adjunctions.") {

    val expect = Stream(Relation(Literal("see"), List(Literal("man"))), Relation(Literal("walk"), List(Literal("man"))), Relation(Literal("walk"), List(Literal("man"), Literal("dog")))).some

    expect assert_=== compile("The man, if the man can see, and if the man can walk, can walk the dog.")

  }

  test("Inverted adjunction.") {

    val expect = Stream(Relation(Literal("walk"), List(Literal("man"))), Relation(Literal("walk"), List(Literal("dog")))).some

    expect assert_=== compile("If the man can walk the dog can walk.")
    expect assert_=== compile("If the man can walk, the dog can walk.")
    expect assert_=== compile("If the man can walk, then the dog can walk.")

  }

  test("ADJP and VP + PP.") {

    val expect = Stream(Relation(Literal("depend"), List(Literal("age disease progression"), Literal("health balance")))).some

    expect assert_=== compile("Age disease progression has depended on the critical and crucial health balance.")
    expect assert_=== compile("Age disease progression depends on the critical and crucial health balance.")
    expect assert_=== compile("Age disease progression has depended on the highly critical, crucial health balance.")
    expect assert_=== compile("Age disease progression has depended on the critical and highly crucial health balance.")
    expect assert_=== compile("Age disease progression has depended on the critical, highly crucial health balance.")
    expect assert_=== compile("Age disease progression also depends on the critical, highly crucial health balance.")

  }

  test("VP + PP") {

    val expect = Stream(Relation(Literal("talk"), List(Literal("man"), Literal("cat")))).some

    expect assert_=== compile("The man can talk about the cat.")

  }

  test("NP + PP and VP + PP.") {

    val expect = Stream(Relation(Literal("depend"), List(Literal("age progression"), Literal("health balance")))).some

    expect assert_=== compile("Disease progression with aging undoubtedly depends on the critical and crucial health balance.")
    expect assert_=== compile("Disease progression with aging depends undoubtedly on the critical and crucial health balance.")

  }

  test("Also.") {

    val expect = Stream(
      Relation(Literal("worsen"), List(Literal("age disease progression"))),
      Relation(Literal("depend"), List(Literal("age disease progression"), Literal("health balance")))
    ).some

    expect assert_=== compile("Age disease progression that can worsen also depends on the critical, highly crucial health balance.")

  }

  test("Which.") {

    val expect = Stream(
      Relation(Literal("be"), List(Literal("age disease progression"))),
      Relation(Literal("depend"), List(Literal("age disease progression"), Literal("health balance")))
    ).some

    expect assert_=== compile("Age disease progression, which is unfortunate, also depends on the critical, highly crucial health balance.")
    expect assert_=== compile("Age disease progression, which is highly unfortunate, also depends on the critical, highly crucial health balance.")

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

    expect assert_=== compile("That type, kind, and class of man and dog can walk the cat, the elephant and the fox.")

  }

  test("Reporting verb + that clause.") {

    val expect = Stream(
      Relation(Literal("suggest"), List(Literal("study"))),
      Relation(Literal("walk"), List(Literal("man"), Literal("dog")))
    ).some

    expect assert_=== compile("The study suggests the man can walk the dog.")
    expect assert_=== compile("The study suggests that the man can walk the dog.")

  }

  test("Nouns & Conjunction Phrases.") {

    val expect = Stream(
      Relation(Literal("walk"), List(Literal("man"))),
      Relation(Literal("walk"), List(Literal("dog"))),
      Relation(Literal("walk"), List(Literal("cat")))
    ).some

    expect assert_=== compile("The man, as well as the dog and the cat can walk.")
    expect assert_=== compile("The man, the dog and the cat can walk.")
    expect assert_=== compile("The man, the dog as well as the cat can walk.")

  }

  test("Parentheses.") {

    val expect = Stream(
      Relation(Literal("walk"), List(Literal("man"))),
      Relation(Literal("walk"), List(Literal("monkey")))
    ).some

    expect assert_=== compile("The man (monkeys) walked.")

  }

  test("Cardinal Numbers.") { 

    val expect = Stream(
      Relation(Literal("participate"), List(
        Literal("66 woman total"),
        Literal("study")
      ))
    ).some

    expect assert_=== compile("A total of 66 women participated in the study.")

  } 

}

