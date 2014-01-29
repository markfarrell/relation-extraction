# Civilize

The goal of this project is to explore ways to make sense of the
overwhelming amount of information available in scientific literature, particularly in the field of gerontology. 

## Dependencies 

- Redis
- SBT 0.13.0 
- Scala 2.10
- Bash
- GNU Make
- Java Runtime Environment

## Install
    ./install.sh
     make

# Tools 

## civilize 

### Synopsis 

    civilize 

### Description

Reads a block of sentences from STDIN.

Writes graph data in GEXF format to STDOUT.

### Design

1. Look for a sentence, S, or a subordinate clause SBAR. 
2. Look for a noun phrase, NP. Flatten this subtree.
3. Look for a verb phrase, VP.
4. If there is a modal verb inside, 
MD, then label subsequent subordinate clauses (starts with SBAR) as conditional.
5. Extract any standalone verbs along the way, tagged with VB, VBZ, VBP, VBD, 
VBN or VBG.
6. Extract any conjunctions (subordinating or preposition), tagged with IN.
7. Repeat / Connect 
    NP -> (MD or None) -> (Any verb form) -> IN -> NP

## berkeley-lazy-cache

### Synopsis

    berkeley-lazy-cache [-clear] [-host <redis-host>] [-port <redis-port>] ... <option-for-berkeley-parser> ...

### Description

Use berkeley-lazy-cache as you would the Berkeley Parser (https://code.google.com/p/berkeleyparser/). It uses a Redis store to cache of parsed sentences. 

It reads from STDIN and writes to STDOUT.


### Options

- clear : Clear the cached data in the redis store
- host <redis-host> : Specify another host for the redis client. Localhost is the default host.
- port <redis-port> : Specify another port for connections to a redis server. The port is 6379 by default.


  

  
  
  


