# A Text-network Compiler

This document is a work-in-progress.

## Abstract

The printed volume of scentific journal articles published each month could likely fill the room you are sitting in right now. It is diffcult to account for all of the findings
presented in these articles. Our goal is to aggregate all of this textual data, to find ways to efficiently analyze its contents and to further research in scientific computing
related to simulating biological processes. The textbook to text-network compiler software should help relieve this issue present in scientific
research.

**General Terms**: Knowledge Representation, Bioinformatics

__Keywords__: Natural Language Processing

**Author**: Mark Farrell, [CSC](http://csclub.uwaterloo.ca).

**Research Director**: Steven A. Garan, [CREA](http://crea.berkeley.edu).

## 1 Introduction

### 1.1 Description

Software for compiling textbooks into text-networks.

### 1.2 Build

To build the software, install SBT 0.13.0+ and then run <code>sbt stage</code>.

### 1.3 Usage

Compile the contents of a textbook into a [GEXF 1.2](http://gexf.net/format/index.html) file, a graph file format.

    ./text-network-compiler -f an_output_file.gexf < an_input_file.txt

## 2 The Software Design

### 2.1 Tokenization
##### To find sentences in a text

The software first tokenizes the text that it reads into individual sentences; a sentence will
contain at least one independent clause, asserting a truth value from a composition of
predicate expressions.

### 2.2 Parsing
##### To describe the grammatical phrase-structure of each sentence

Each sentence is piped through the [The Berkeley Parser](http://nlp.cs.berkeley), to generate
an abstract syntax tree that describes its phrase-structure. See [Penn Treebank II Constituent Tags](http://www.surdeanu.info/mihai/teaching/ista555-fall13/readings/PennTreebankConstituents.html).

### 2.3 Compilation
##### To find simple predicate expressions in sentences

#### 2.3.1 Definitions

##### 2.3.1.1 Constituents
##### 2.3.1.2 Phrases
##### 2.3.1.3 Clauses
##### 2.3.1.4 Simple Predicate Expressions

#### 2.3.2 Phrase-Structure Patterns

##### 2.3.2.1 Predicate Arguments
##### 2.3.2.2 Monovalent Predicate Expressions
##### 2.3.2.3 Divalent Predicate Expressions


Mammals make five classes of antibodies, each of which mediates a characteristic biological response following antigen binding.

    (ROOT
      (S
        (@S
          (NP (NNS Mammals))
          (VP (VBP make)
            (NP
              (@NP
                (@NP
                  (NP (CD five) (NNS classes))
                  (PP (IN of)
                    (NP (NNS antibodies))))
                (, ,))
              (SBAR
                (WHNP (DT each)
                  (WHPP (IN of)
                    (WHNP (WDT which))))
                (S
                  (VP
                    (@VP (VBZ mediates)
                      (NP
                        (@NP
                          (@NP (DT a) (JJ characteristic))
                          (JJ biological))
                        (NN response)))
                    (PP (VBG following)
                      (NP (NN antigen) (JJ binding)))))))))
        (. .)))

Predicate Expressions:

    make(mammal, class)
    make(mammal, antibody)
    follow(response, antigen)
    mediate(class, response)
    mediate(antibody, response)
    has(antibody, class)

##### 2.3.2.4 Trivalent Predicate Expressions

The man gave the dog his food.

     (ROOT
        (S
          (@S
            (NP (DT The) (NN man))
            (VP
              (@VP (VBD gave)
                (NP (DT the) (NN dog)))
                (NP (PRP$ his) (NN food))))
          (. .)))

##### 2.3.2.5 Ignored Constituents

###### 2.3.2.5.1 Adjective Phrases (ADJP)


The efficiency of antigen binding and cross-linking is greatly increased by a flexible hinge region in most antibodies, which allows the distance between the two antigen-binding sites to vary ( Figure 24-20 ).

    (...
      (NP
        (NP (DT The) (NN efficiency))
        (PP (IN of)
          (NP
            (NP (NN antigen))
            (ADJP
              (@ADJP (JJ binding)
                (CC and))
              (JJ cross-linking)))))
      ...)

##### 2.3.2.5 Nonfinite Verb Phrases
##### 2.3.2.6 Declarative Clauses Containing a Noun Phrase and Verb Phrase
##### 2.3.2.7 Noun Phrases Followed by a Subordinate Clause

### 2.4 Examples

#### 2.4.1 Example

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

#### 2.4.2 Example

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

## 3 Conclusions

## 4 Recommendations

 * Use a hypergraph structure (edges between edges) to compose predicate expressions.
 * Write software that uses these text-networks to generate [CREAL](http://www.fasebj.org/cgi/content/meeting_abstract/26/1_MeetingAbstracts/717.3?sid=d7799d6d-aa85-4442-9055-3e2d97332c94):
a language for describing biological systems from a macro to a molecular scale.

