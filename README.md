# Text-network Compiler

#### Towards Simulating Biological Systems from Descriptions in Raw Text

Produces text-networks from the contents of textbooks.

To build the software, install SBT 0.13.0+ and then run <code>sbt compile</code>.

**General Terms**: Knowledge Representation, Biology Research

__Keywords__: Natural Language Processing

**Author**: Mark Farrell, [CSC](http://csclub.uwaterloo.ca).

**Research Director**: Steven A. Garan, [CREA](http://crea.berkeley.edu).

## Usage

Compile the contents of a textbook into a [GEXF 1.2](http://gexf.net/format/index.html) file, a graph file format.

    ./Compiler -f an_output_file.gexf < an_input_file.txt

## How It Works

### Example

#### Input

Sentence:

Virulence genes are frequently clustered together, either in groups on the bacterial chromosome called pathogenicity islands or on extrachromosomal virulence plasmids ( Figure 25-5 ).

#### Parser Result

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

See [Penn Treebank II Constituent Tags](http://www.surdeanu.info/mihai/teaching/ista555-fall13/readings/PennTreebankConstituents.html).

#### Compilation Result

Propositions:

    be(gene, gene)
    cluster(gene, virulence plasmid)
    cluster(gene, chromosome)
    cluster(gene, pathogenicity island)
    has(chromosome, group)
    has(pathogenicity island, group)
    call(chromosome, pathogenicity island)








