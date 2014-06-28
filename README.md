# Text-network Compiler

#### Towards Simulating Biological Systems from Descriptions in Raw Text

Software for compiling textbooks into text-networks.
To build the software, install SBT 0.13.0+ and then run <code>sbt compile start-script</code>.

**General Terms**: Knowledge Representation, Bioinformatics

__Keywords__: Natural Language Processing

**Author**: Mark Farrell, [CSC](http://csclub.uwaterloo.ca).

**Research Director**: Steven A. Garan, [CREA](http://crea.berkeley.edu).

This document is a work-in-progress.

## Usage

Compile the contents of a textbook into a [GEXF 1.2](http://gexf.net/format/index.html) file, a graph file format.

    ./Compiler -f an_output_file.gexf < an_input_file.txt

## How It Works

### 2.1 Tokenization
##### For finding individual sentences in a text

The software first tokenizes the text that it reads into individual sentences; a sentence will
contain at least one independent clause, asserting a truth value from a composition of
predicate expressions.

### 2.2 Parsing
##### To describe the grammatical structure of each sentence

Each sentence is piped through the [The Berkeley Parser](http://nlp.cs.berkeley), to generate
an abstract syntax tree that describes its phrase-structure. See [Penn Treebank II Constituent Tags](http://www.surdeanu.info/mihai/teaching/ista555-fall13/readings/PennTreebankConstituents.html).

### 2.3 Compilation
##### To find and represent predicate functions and their arguments

This section is under construction.

### 2.4 Examples

#### Example 1

##### Input

Sentence:

These monomers are strung together in a long linear sequence that encodes the genetic information, just as the sequence of 1s and 0s encodes the information in a computer file.

##### Parser Result

Phrase-structure tree:

    (ROOT
      (S
        (@S
          (NP (DT These) (NNS monomers))
          (VP (VBP are)
            (VP
              (@VP
                (@VP
                  (@VP (VBN strung)
                    (ADVP (RB together)))
                  (PP (IN in)
                    (NP
                      (NP
                        (@NP
                          (@NP (DT a) (JJ long))
                          (JJ linear))
                        (NN sequence))
                      (SBAR
                        (WHNP (WDT that))
                        (S
                          (VP (VBZ encodes)
                            (NP
                              (@NP (DT the) (JJ genetic))
                              (NN information))))))))
                (, ,))
              (SBAR
                (@SBAR (RB just) (IN as))
                (S
                  (NP
                    (NP (DT the) (NN sequence))
                    (PP (IN of)
                      (NP
                        (@NP (NN 1s)
                          (CC and))
                        (NN 0s))))
                  (VP
                    (@VP (VBZ encodes)
                      (NP (DT the) (NN information)))
                    (PP (IN in)
                      (NP
                        (@NP (DT a) (NN computer))
                        (NN file)))))))))
        (. .)))



##### Compilation Result

Predicate Expressions:

    string(monomer, monomer)
    encode(monomer, information)
    has(computer file, information)
    encode(1 0, information)
    has(1 0, sequence)

#### Example 2

##### Input

Sentence:

Virulence genes are frequently clustered together, either in groups on the bacterial chromosome called pathogenicity islands or on extrachromosomal virulence plasmids ( Figure 25-5 ).

##### Parser Result

Phrase-structure tree:

    (ROOT
      (S
        (@S
          (NP (JJ Virulence) (NNS genes))
          (VP
            (@VP (VBP are)
              (ADVP (RB frequently)))
            (VP
              (@VP
                (@VP (VBN clustered)
                  (ADVP (RB together)))
                (, ,))
              (PP
                (@PP
                  (@PP (CC either)
                    (PP (IN in)
                      (NP
                        (NP (NNS groups))
                        (PP (IN on)
                          (NP
                            (NP
                              (@NP (DT the) (JJ bacterial))
                              (NN chromosome))
                            (VP (VBN called)
                              (S
                                (NP (NN pathogenicity) (NNS islands)))))))))
                  (CC or))
                (PP (IN on)
                  (NP
                    (NP
                      (@NP (JJ extrachromosomal) (NN virulence))
                      (NNS plasmids))
                    (PRN
                      (@PRN (-LRB- -LRB-)
                        (NP (NN Figure) (CD 25-5)))
                      (-RRB- -RRB-))))))))
        (. .)))

##### Compilation Result

Predicate Expressions:

    be(gene, gene)
    cluster(gene, virulence plasmid)
    cluster(gene, chromosome)
    cluster(gene, pathogenicity island)
    has(chromosome, group)
    has(pathogenicity island, group)
    call(chromosome, pathogenicity island)

## Recommendations

 * Use a hypergraph structure (edges between edges) to compose predicate expressions.
 * Write software that uses these text-networks to generate [CREAL]((http://www.fasebj.org/cgi/content/meeting_abstract/26/1_MeetingAbstracts/717.3?sid=d7799d6d-aa85-4442-9055-3e2d97332c94):
a language for describing biological systems from a macro to a molecular scale.





