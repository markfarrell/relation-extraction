# Beagle

Take blocks of text and produce visual aids to study the properties of topics and the relations between
them. Affiliated with [CREA](http://crea.berkeley.edu/). Type <code>./Beagle --help</code> for 
more information once built. 

## Dependencies 

- SBT 0.13.0 
- Scala 2.10
- PostgreSQL 9.3.3 
- Java Runtime Environment 1.7.x
- GNU Make

This software produces [GEXF 1.2](http://gexf.net/format/index.html) files, viewable in
[Gephi 0.8.2-beta](http://gephi.org/users/download/) and most likely other revisions. 

## Build
    make
    make samples

Tests are run as part of the build process. 

## Setup

Clone this repository into a directory, which this guide refers to as BEAGLE_DIR.
Have a Java 1.7+ JRE and GNU Make installed on your machine.
Download archived releases of [SBT 0.13.0](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html), 
[Scala 2.10](http://www.scala-lang.org/download/) 
and [PostgreSQL 9.3.3](http://git.postgresql.org/gitweb/?p=postgresql.git;a=commit;h=0691fe504723c06ce6ccd1de257fe212609beb13). Extract the SBT and Scala, archives in your preferred location. Then, prepend your PATH environment variable with their bin directories. Afterwards, checkout the commit to the Postgres git repository tagged REL_9_3_3. Using your shell, <code>./configure ... </code> your build and then run <code>make</code> followed by <code>make install</code>. 
Add the built bin directory your path. Run <code>initdb</code> and configure your <code>PGDATA</code> environment variable. Once completed, <code>createdb</code> a database called <code>test</code>. 
Now, run <code>psql -f BEAGLE_DIR/install/create.sql -d test ... </code>, so that this software may be built and tested.




  

  
  
  


