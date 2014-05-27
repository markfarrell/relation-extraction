all: build

build:
	export SBT_OPTS="-Xss1m -XX:MaxPermSize=2048m -XX:+CMSClassUnloadingEnabled"
	sbt compile start-script test

bin:
	mkdir -p bin

%.gexf: %.txt
	./Compiler -f bin/$(notdir $@) < $<

samples: bin $(patsubst %.txt, %.gexf, $(wildcard samples/*.txt))

clean:
	sbt clean
	rm -rf bin

.PHONY: all build grammar clean
