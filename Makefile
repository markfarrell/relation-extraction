all: 
	sbt compile start-script test

clean:
	sbt clean	

.PHONY: clean
