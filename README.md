berkeley-parser-memoize

# Synopsis

    berkeley-parser-memoize [-clear] [-host <redis-host>] [-port <redis-port>] ... <option-for-berkeley-parser> ...

# Description

A tool with the same interface as the berkeley-parser (https://code.google.com/p/berkeleyparser/). It keeps sentence-tree key-value pairs in memory, improving running time by getting rid of duplicate computations that might plague a project. Particularly, it should be useful to those who want to perform map, filter and fold operations
on a large volume of sentence-tree pairs. 

It reads from STDIN and writes to STDOUT.


# Options

- clear : Clear the cached data in the redis store
- host <redis-host> : Specify another host for the redis client. Localhost is the default host.
- port <redis-port> : Specify another port for connections to a redis server. The port is 6379 by default.


# Dependencies 

- A redis server for the tool to connect to. 
- Scala 2.10
- Bash
- GNU Make
- Java Runtime Environment

# Install

  make
  
# Run

  ./berkeley-parser-memoize
  
  
  


