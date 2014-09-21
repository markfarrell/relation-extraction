
![travis-ci](https://travis-ci.org/crea-berkeley/knowledge-extraction.svg?branch=master)

# Knowledge Extraction Software

Compiles text into graphs, relating literal nouns by the actions they perform on each other.

#### Instructions

##### Build

Install SBT 0.13.0+ and then run <code>sbt stage</code>.

##### Usage

Compile text into a [GEXF 1.2](http://gexf.net/format/index.html) file, a graph file format.

    ./crea-compile -f an_output_file.gexf < an_input_file.txt

#### Demonstration

 [![Results](results.png)](http://markfarrell.ca/creal)




