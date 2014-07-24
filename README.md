
# A Text-network Compiler
###### For building knowledge bases that can be used to describe complex systems.

This document is a work-in-progress.

## Abstract

  It is the aspiration of the Center for Research and Education On Aging [(CREA)](http://crea.berkeley.edu) to build a knowledge base that can be used by a system to simulate the human aging process at all levels of abstraction. There is a need for a tool that can automate the construction of the knowledge base; reading text articles and constructing it solely by hand is infeasible. A software tool was developed that can compile text into text-networks. The text-networks can be used to generate [CREAL](http://crea.berkeley.edu/FASEB_POSTER2012_FINAL_FINAL_GOLD_VLSB_yellow_title_box_56x36_PDF.pdf) documents, a declarative language that will be used within the knowledge base to describe the human aging process. Clustering algorithms, such as Chinese Whispers, can be applied to the text-networks, revealing properties of human organs, actions that can be performed (on other organs), their locations in the human body and also alternative names used in literature to describe them; this information is needed to generate CREAL documents. The tool can lead to significant process in building the knowledge base on aging, and consequently will provide new insight into the phenomena that drive the aging process.

**General Terms**: Natural Language Processing, Bioinformatics

__Keywords__: Semantic Parsing, Clustering Algorithms, Spam Filtering

**Author**: Mark Farrell

**Research Director**: Steven A. Garan

## Build

To build the current software, install SBT 0.13.0+ and then run <code>sbt stage</code>.

## Usage

Compile the contents of a textbook into a [GEXF 1.2](http://gexf.net/format/index.html) file, a graph file format.

    ./text-network-compiler -f an_output_file.gexf < an_input_file.txt

## Contribute

 If you're interested in contributing to the project, feel free to post your questions and
 discuss your plans on both the IRC channel and mailing list.

 * IRC channel: <code>#crea</code> on <code>irc.freenode.net</code>.
 * Mailing list: <code>crea-berkeley@googlegroups.com</code>.

## Resources

   Here's a set of links to articles and content that should be explored:

 *  The Open Biological and Biomedical Ontologies - http://www.obofoundry.org/.
 *  The paper titled "Semantic Parsing using Distributional Semantics and Probabilistic Logic" - http://sp14.ws/pub/bem-sp14-2014.pdf.
 * Chinese whispers: an efficient graph clustering algorithm and its application to natural language processing problems - http://dl.acm.org/citation.cfm?id=1654774




