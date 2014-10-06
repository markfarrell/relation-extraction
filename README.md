![travis-ci](https://travis-ci.org/crea-berkeley/knowledge-extraction.svg?branch=master)

# Knowledge Extraction Software

Extracts knowledge from text articles.

#### Instructions

Install SBT 0.13.0+.

##### Usage

    $ sbt console
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

    scala> Gexf(compiled)
    res0: Tree[String] \/ Elem = \/-(
      <gexf version="1.2" xmlns="http://www.gexf.net/1.2draft">
        <graph mode="static" idtype="string" defaultedgetype="directed">
          <nodes count="2">
            <node label="man" id="man"/>
            <node label="dog" id="dog"/>
          </nodes>
          <edges count="1">
            <edge label="walk" type="directed" source="man" target="dog" id="87c3..."/>
          </edges>
        </graph>
      </gexf>
    )
