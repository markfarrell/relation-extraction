
BERKELEY_BIN := ./bin
BERKELEY_SRC_BASE := ./src/edu/berkeley/nlp/parser

all: $(BERKELEY_BIN)
	scalac -cp "./lib/slf4j-1.7.5/*:./lib/*" -d $(BERKELEY_BIN) $(BERKELEY_SRC_BASE)/*

$(BERKELEY_BIN): 
	mkdir -p $(BERKELEY_BIN)

clean: $(BERKELEY_BIN)
	rm -rf $(BERKELEY_BIN)


.PHONY: clean
