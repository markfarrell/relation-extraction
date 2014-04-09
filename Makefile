all:
	export SBT_OPTS="-Xss1m -XX:MaxPermSize=2048m"
	sbt compile start-script test

bin: 
	mkdir -p bin

%.gexf: %.txt
	./Beagle --clear --export -f bin/$(notdir $@) < $<

samples: bin $(patsubst %.txt, %.gexf, $(wildcard samples/*.txt))

schema: 
	psql -d test -f ./install/clean.sql
	psql -d test -f ./install/create.sql

clean:
	sbt clean
	rm -rf bin

.PHONY: all sample clean
