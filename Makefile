all:
	export SBT_OPTS="-Xss1m"
	sbt compile start-script test

clean:
	sbt clean	

.PHONY: clean
