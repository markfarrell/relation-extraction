![travis-ci](https://travis-ci.org/crea-berkeley/knowledge-extraction.svg?branch=master)

# Relation Extraction Software

Extracts relations from text articles.

#### Instructions

Install SBT 0.13.0+.

##### Usage

    $ sbt console
```scala
scala> val sentence = "The man walks the dog."
sentence: String = The man walks the dog.

scala> val parsed = Parse(sentence)
parsed: Tree[String] =
  (ROOT
    (S
      (@S
        (NP (DT The) (NN man))
        (VP (VBZ walks)
          (NP (DT the) (NN dog))))
      (. .)))

scala> val compiled = Compile(parsed)
compiled: Tree[String] \/ List[Relation] = \/-([
  <relation:walk(<literal:man>, <literal:dog>)>
])
```

[Wiki](https://github.com/crea-berkeley/knowledge-extraction/wiki)
