all: build 

build:
	export SBT_OPTS="-Xss1m -XX:MaxPermSize=2048m -XX:+CMSClassUnloadingEnabled"
	sbt compile start-script test

bin: 
	mkdir -p bin

%.gexf: %.txt
	./Beagle -f bin/$(notdir $@) < $<

samples: bin $(patsubst %.txt, %.gexf, $(wildcard samples/*.txt))

grammar: bin
	./Beagle --dumpgrammar > bin/grammar.html
	xmllint --output bin/grammar.html --format - < bin/grammar.html

schema: 
	psql -d test -f ./install/clean.sql
	psql -d test -f ./install/create.sql

clean:
	sbt clean
	rm -rf bin

.PHONY: all build grammar clean
