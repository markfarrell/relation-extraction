
# A Text-network Compiler

This document is a work-in-progress.

## Abstract

The printed volume of scentific journal articles published each month could likely fill the room you are sitting in right now. When doing research, it is difficult to account for all of the findings
presented in these articles. Aggregating text and visualizing it as a text-network should help to relieve this issue.
In particular, the intention is to use this software as an aid for constructing an ontology in the domain of aging research. 

**General Terms**: Knowledge Representation, Bioinformatics

__Keywords__: Natural Language Processing

**Author**: Mark Farrell, [CSC](http://csclub.uwaterloo.ca).

**Research Director**: Steven A. Garan, [CREA](http://crea.berkeley.edu).

## 1 Introduction

Software for compiling raw text into text-networks should provide some
relief for researchers dealing with the large quantity of information published in
scientific journal articles. The project should be composed of four software components:

 1. Software for aggregating journal articles and textbooks in raw file format from a variety of sources.
 2. Software that compiles text articles into [Datalog](http://en.wikipedia.org/wiki/Datalog), expressing
    facts and rules derived from first-order logic.
 3. Software to interpret Datalog environments and visualize them (as text-networks).
 4. Software to store, search and retrieve Datalog environments that were compiled.

Currently, there is only software to compile text into text-networks; separate components
for encoding first-order logic and data visualization are being worked on right now. There is
yet to be progress made on items 1 and 4.

### 1.1 Build

To build the current software, install SBT 0.13.0+ and then run <code>sbt stage</code>.

### 1.2 Usage

Compile the contents of a textbook into a [GEXF 1.2](http://gexf.net/format/index.html) file, a graph file format.

    ./text-network-compiler -f an_output_file.gexf < an_input_file.txt

### 1.3 Contributing

 If you're interested in contributing to the project, feel free to post your questions and
 discuss your plans on both the IRC channel and mailing list.

 * IRC channel: <code>#crea</code> on <code>irc.freenode.net</code>.
 * Mailing list: <code>crea-berkeley@googlegroups.com</code>.

## 2 The Software Design

### 2.1 Tokenization
##### To find sentences in a text

The software first tokenizes the text that it reads into individual sentences; a sentence will
contain at least one independent clause.

### 2.2 Parsing
##### To describe the grammatical phrase-structure of each sentence

Each sentence is piped through the [The Berkeley Parser](http://nlp.cs.berkeley) to generate
a tree of constituents.

### 2.3 Compilation
##### To find atoms and compound terms in sentences

#### 2.3.1 Definitions

##### 2.3.1.1 Constituents

Constituents are nonterminal nodes of the trees produced by The Berkeley Parser. Each constituent
has a tag, describing the syntactic function of the subtree in relation to the sentence that was
parsed. Constituents are either words, phrases or clauses. See [Penn Treebank II Constituent Tags](http://www.surdeanu.info/mihai/teaching/ista555-fall13/readings/PennTreebankConstituents.html) for details and examples.

##### 2.3.1.2 Phrases

Phrases are constituents that group together several words, but do not form a complete proposition.

##### 2.3.1.3 Clauses

Clauses, in constrast to phrases, are groups of several constituents that form a complete proposition; they can be simple, made up of phrases, or compounded with other clauses.

##### 2.3.1.4 First-order Logic

  The compiler implements a [zipper](http://www.haskell.org/haskellwiki/Zipper) that takes a tree of constituents and rebuilds a tree composed of [terms](http://en.wikipedia.org/wiki/Prolog): atoms and compounds terms. A set of patterns are matched on at each level of the constituent tree, whereafter atoms and compound terms are constructed from [predicates](http://en.wikipedia.org/wiki/Predicate_%28mathematical_logic%29) and arguments that are found in text.

#### 2.3.2 Constituent Patterns

The set of consituent patterns form a disjoint union: i.e. each pattern that is looked for
at each level in the tree is unique. A disjoint set of constituent-pattern objects is
implemented in Scala:

    sealed trait ConstituentPattern
    private[this] object PredicateArgument extends ConstituentPattern { ... }
    private[this] object MonovalentPredicate extends ConstituentPattern { ... }
    private[this] object DivalentPredicate extends ConstituentPattern { ... }
    private[this] object TrivalentPredicate extends ConstituentPattern { ... }
    private[this] object LogicalConnective extends ConsitutentPattern { ... }
    private[this] object IgnoredConstituent extends ConstituentPattern { ... }

##### 2.3.2.1 Predicate Arguments

Atom terms are constructed when predicate arguments are found in constituent trees.

    private[this] object PredicateArgument extends ConstituentPattern {

      def unapply(tree : Tree[String]) : Option[Atom] = tree match {
        case Tree.Node("NP", Stream("NN", "NNS")) => Atom(tree).some
        case Tree.Node("NP", Stream("NN", "NN")) => Atom(tree).some
        case Tree.Node("NP", Stream("NN", "NNPS")) => Atom(tree).some
        case Tree.Node("NP", Stream("NNP", "NNS")) => Atom(tree).some
        case Tree.Node("NP", Stream("NNP", "NN")) => Atom(tree).some
        case Tree.Node("NP", Stream("NNP", "NNPS")) => Atom(tree).some
        case Tree.Node("NP", Stream("@NP", "NNP")) => Atom(tree).some
        case Tree.Node("NP", Stream("@NP", "NN")) => Atom(tree).some
        case Tree.Node("NP", Stream("@NP", "NNPS")) => Atom(tree).some
        case Tree.Node("NN", Stream()) => Atom(tree).some
        case Tree.Node("NNS", Stream()) => Atom(tree).some
        case Tree.Node("NNP", Stream()) => Atom(tree).some
        case Tree.Node("NNPS", Stream()) => Atom(tree).some
        case _ => none
      }

    }

##### 2.3.2.2 Monovalent Predicate Expressions

A monovalent predicate takes one argument. Patterns can be built in Scala to find monovalent predicates and their arguments in constituent trees.
Let's look at a couple of examples of sentences containing monovalent predicates:

A sentence with one simple declarative clause:

The man sleeps.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (VBZ sleeps)))
        (. .)))

Patterns:

    // Monovalent Predicate from 3rd-person singular present verb, past tense verb, ...
    Tree.Node("S", Stream(Tree.Node("NP", x), Tree.Node("VP", Stream(Tree.Node(_, y)))))
    Tree.Node("@S", Stream(Tree.Node("NP", x), Tree.Node("VP", Stream(Tree.Node(_, y)))))

Here, <code>sleeps</code> is the monovalent predicate and <code>man</code> is the argument
applied to it to form a clause. Another example of a sentence with conjoined simple declarative clauses:

The man sleeps, the dog walks and the cat eats.

    (ROOT
      (S
        (@S
          (@S
            (@S
              (@S
                (S
                  (NP (DT The) (NN man))
                  (VP (VBZ sleeps)))
                (, ,))
              (S
                (NP (DT the) (NN dog))
                (VP (VBZ walks))))
            (CC and))
          (S
            (NP (DT the) (NN cat))
            (VP (VBZ eats))))
        (. .)))

It seems like the same patterns will hold for a list of conjoined simple declarative clauses.

Consider other sentences with the same predicate and argument, expressed with a different verb tense:

The man slept.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (VBD slept)))
        (. .)))

No new patterns when the past tense verb <code>slept</code> is used.

The man is sleeping.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (VBZ is)
            (VP (VBG sleeping))))
        (. .)))

The man was sleeping.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (VBD was)
            (VP (VBG sleeping))))
        (. .)))


Patterns:

    // Monovalent Predicate from present continuous verb, past continuous verb, ...
    Tree.Node("S", Stream(Tree.Node("NP", x), Tree.Node("VP", Stream(_, Tree.Node("VBG", y)))))
    Tree.Node("@S", Stream(Tree.Node("NP", x), Tree.Node("VP", Stream(_, Tree.Node("VBG", y)))))


Examine sentences with adverbial phrases inserted:

The man sleeps freely.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (VBZ sleeps)
            (ADVP (RB freely))))
        (. .)))

Patterns:

    // Monovalent Predicate from verb with adverbial phrase appended
    Tree.Node("S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node(_, y),
        Tree.Node("ADVP", _)
      ))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node(_, y),
        Tree.Node("ADVP", _)
      ))
    ))

The man freely sleeps.

    (ROOT
      (S
        (@S
          (@S
            (NP (DT The) (NN man))
            (ADVP (RB freely)))
          (VP (VBZ sleeps)))
        (. .)))

Patterns:

    // Monovalent Predicate from verb with adverbial phrase prepended
    Tree.Node("S", Stream(
      Tree.Node("@S", Stream(
        Tree.Node("NP", x),
        Tree.Node("ADVP", _)
      )),
      Tree.Node("VP", Stream(Tree.Node("VBZ", y)))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("@S", Stream(
        Tree.Node("NP", x),
        Tree.Node("ADVP", _)
      )),
      Tree.Node("VP", Stream(Tree.Node("VBZ", y)))
    ))

Sometimes a modal verb will be inserted:

The man might sleep.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB sleep))))
        (. .)))

Patterns:

    // Monovalent Predicate from modal verb governing main verb
    Tree.Node("S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node(_, y)
      ))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node(_, y)
      ))
    ))

There are also phrasal verbs, combining a verb and a particle to form a new verb:

The man takes off.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (VBZ takes)
            (PRT (RP off))))
        (. .)))

Patterns:

    // Monovalent Predicate from phrasal verb
    Tree.Node("S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node(_, y),
        Tree.Node("PRT", z)
      ))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node(_, y),
        Tree.Node("PRT", z)
      ))
    ))

Here are some slightly more complex ways to express the same facts about the <code>man</code>:

The man might sleep quietly.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB sleep)
              (ADVP (RB quietly)))))
        (. .)))

Patterns:

    Tree.Node("S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(_,
        Tree.Node("VP", Stream(y,
          Tree.Node("ADVP", _)
        ))
      ))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(_,
        Tree.Node("VP", Stream(y,
          Tree.Node("ADVP", _)
      ))
    ))

The man might quietly sleep.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP
            (@VP (MD might)
              (ADVP (RB quietly)))
            (VP (VB sleep))))
        (. .)))

Patterns:

    Tree.Node("S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("@VP", Stream(
          Tree.Node("MD", _),
          Tree.Node("ADVP", _)
        )),
        Tree.Node("VP", y)
      ))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("@VP", Stream(
          Tree.Node("MD", _),
          Tree.Node("ADVP", _)
        )),
        Tree.Node("VP", y)
      ))
    ))

The man might take off.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB take)
              (PRT (IN off)))))
        (. .)))

Patterns:

    Tree.Node("S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(_,
        Tree.Node("VP", Stream(y,
          Tree.Node("PRT", _)))
      ))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(_,
        Tree.Node("VP", Stream(y,
          Tree.Node("PRT", _)))
      ))
    ))

The man might have taken off.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB have)
              (VP (VBN taken)
                (PRT (RP off))))))
        (. .)))

Patterns:

    Tree.Node("S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node("VP", Stream(
          Tree.Node("VB", _),
          Tree.Node("VP", Stream(y,
            Tree.Node("PRT", _)
          ))
        ))
      ))
    ))

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node("VP", Stream(
          Tree.Node("VB", _),
          Tree.Node("VP", Stream(y,
            Tree.Node("PRT", _)
          ))
        ))
      ))
    ))

Modal present perfect tense:

The man might have slept.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB have)
              (VP (VBD slept)))))
        (. .)))

Patterns:

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node("VP", Stream(
          Tree.Node("VB", _),
          Tree.Node("VP", Stream(y))
        ))
      ))
    ))

Modal present perfect tense with adverb prepended:

The man might have quietly slept.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB have)
              (VP
                (ADVP (RB quietly))
                (VBN slept)))))
        (. .)))

Patterns:

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("VB", _),
        Tree.Node("VP", Stream(
          Tree.Node("ADVP", _),
          y
        ))
      ))
    ))

Modal present perfect tense with adverb appended:

The man might have slept quietly.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB have)
              (VP (VBD slept)
                (ADVP (RB quietly))))))
        (. .)))


Patterns:

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node("VP", Stream(
          Tree.Node("VB", _),
          Tree.Node("VP", Stream(
            Tree.Node("VBD", Stream(y)),
            Tree.Node("ADVP", _)
          ))
        ))
      ))
    ))

Modal present perfect tense phrasal verb with adverb prepended:

The man might have quietly taken off.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP
              (@VP (VB have)
                (ADVP (RB quietly)))
              (VP (VBN taken)
                (PRT (RP off))))))
        (. .)))

Patterns:

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node("VP", Stream(
          Tree.Node("@VP", Stream(
            Tree.Node("VB", _),
            Tree.Node("ADVP", _)
          )),
          Tree.Node("VP", Stream(
            Tree.Node(_, Stream(_)),
            Tree.Node("PRT", _)
          ))
        ))
      ))
    ))

Modal present perfect tense phrasal verb with adverb appended:

The man might have taken off quietly.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP (MD might)
            (VP (VB have)
              (VP
                (@VP (VBN taken)
                  (PRT (RP off)))
                (ADVP (RB quietly))))))
        (. .)))

Patterns:

    Tree.Node("@S", Stream(
      Tree.Node("NP", x),
      Tree.Node("VP", Stream(
        Tree.Node("MD", _),
        Tree.Node("VP", Stream(
          Tree.Node("VB", Stream(_))
        ))
      ))
    ))



The patterns match subtrees of constituents that can be used to construct a compound term from a monovalent predicate and an argument:

    private[this] object MonovalentPredicate extends ConstituentPattern {

      def unapply(tree : Tree[String]) : Option[CompoundTerm] = tree match {
        case ...  => x match {
          case PredicateArgument(term) => (CompoundTerm(y, Stream(atom))).some
        }
        case _ => none
      }

    }

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

Facts:

    make(mammal, class).
    make(mammal, antibody).
    follow(response, antigen).
    mediate(class, response).
    mediate(antibody, response).
    has(class, antibody).

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

Facts:

    give(man, dog, food).

##### 2.3.2.5 Ignored Constituents

Here is a list of constituents ignored by the compiler:

###### Clause Constituents

 - SQ (Direct Question)
 - SBARQ (Indirect Question)
 - SINV (Inverted Clause)

###### Phrase Constituents

 - ADJP (Adjective Phrase)
 - ADVP (Adverbial Phrase)
 - QP (Quantitative Phrase)
 - PRN (Parenthetical Phrase)

###### Word Constituents

 - DT (Noun Determiner)
 - JJ (Adjective)
 - JJS (Adjective Superlative)
 - JJR (Adjective Comparative)
 - PRP (Personal Pronoun)
 - PRP$ (Possessive Pronoun)
 - UH (Interjection)

Here is the pattern's implementation:

    object IgnoredConstituent extends ConstituentPattern {

      def unapply(tree : Tree[String]) : Boolean = tree.loc.getLabel match {
        case "SINV" | "SBARQ" | "SQ" => true
        case "ADJP" | "ADVP" | "X" | "NX" | "QP" | "PRN" => true
        case "DT" | "JJ" | "JJS" | "JJR" | "PRP" | "PRP$" | "UH" => true
        case _ => false
      }

    }

###### 2.3.2.5.1 Adjective Phrases (ADJP)

It would be nice to be able to produce the facts <code>has(antigen, binding).</code> and <code>has(antigen, cross-linking).</code> from the adjective phrase found in this sentence:

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

#### 2.3.2.5 Prepositional phrases

[Prepositional phrases](http://www.chompchomp.com/terms/prepositionalphrase.htm) function the same as adjectives and adverbs.

Noun phrases can also be composed of another noun phrase and a prepositional phrases. A list of compound terms should be constructed from such a pattern. Consider an example sentence:

That type of man sleeps.

    (ROOT
      (S
        (@S
          (NP
            (NP (DT That) (NN type))
            (PP (IN of)
              (NP (NN man))))
          (VP (VBZ sleeps)))
        (. .)))

<code>has(type, man).</code> should be a fact found in this sentence.

#### 2.3.2.6 Negated Clauses

Negated clauses are currently not handled by the software.

Here are a couple examples of negated simple declarative clauses:

The man did not sleep.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP
            (@VP (VBD did) (RB not))
            (VP (VB sleep))))
        (. .)))

The man might not have slept.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP
            (@VP (MD might) (RB not))
            (VP (VB have)
              (VP (VBD slept)))))
        (. .)))


#### 2.3.2.7 Logical Connectives

##### 2.3.2.7.1 Implications

##### 2.3.2.7.2 Conjunctions

## 3 Conclusions

## 4 Recommendations

 * Visualize logical implications using a hypergraph structure (edges between edges).
 * Write software that uses these text-networks to generate [CREAL](http://www.fasebj.org/cgi/content/meeting_abstract/26/1_MeetingAbstracts/717.3?sid=d7799d6d-aa85-4442-9055-3e2d97332c94):
a language for describing biological systems from a macro to a molecular scale.

### 4.1 Resources 

   Here's a set of links to articles and content that should be explored:

 *  The Open Biological and Biomedical Ontologies - http://www.obofoundry.org/.
 *  The paper titled "Semantic Parsing using Distributional Semantics and Probabilistic Logic" - http://sp14.ws/pub/bem-sp14-2014.pdf. 





