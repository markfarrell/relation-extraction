all:
	export SBT_OPTS="-Xss1m"
	sbt compile start-script test

sample: $(patsubst %.txt, %.gexf, $(wildcard samples/*.txt))
	zip samples/samples.zip $<
	rm -f samples/*.gexf

%.gexf: %.txt
	./Beagle -f $@ < $<

clean:
	sbt clean	

.PHONY: clean
