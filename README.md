# Beagle

An attempt to make sense of the vast amount of information available in scientific literature.

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

## berkeley-lazy-cache

### Synopsis

    berkeley-lazy-cache [-clear] [-host <redis-host>] [-port <redis-port>] ... <option-for-berkeley-parser> ...

### Description

Use berkeley-lazy-cache as you would the Berkeley Parser (https://code.google.com/p/berkeleyparser/). It uses a Redis store to cache the output when sentences are parsed.

It reads from STDIN and writes to STDOUT.

### Options

- clear : Clear the cached data in the redis store
- host <redis-host> : Specify another host for the redis client. Localhost is the default host.
- port <redis-port> : Specify another port for connections to a redis server. The port is 6379 by default.


  

  
  
  


