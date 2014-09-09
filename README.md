
![travis-ci](https://travis-ci.org/markfarrell/crea-nlp.svg?branch=master)

# crea-nlp

Information extraction software - for automatically constructing knowledge bases.

#### Instructions

##### Build

To build the current software, install SBT 0.13.0+ and then run <code>sbt stage</code> from the <code>crea-compile</code> directory.

##### Usage

Compile the contents of a textbook into a [GEXF 1.2](http://gexf.net/format/index.html) file, a graph file format.

    ./crea-compile -f an_output_file.gexf < an_input_file.txt

#### Demonstration

 [![Results](results.png)](http://markfarrell.ca/creal)




