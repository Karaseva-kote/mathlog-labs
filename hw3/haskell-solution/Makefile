HC=ghc
SOURCES=src/Main.hs src/Grammar.hs src/Axioms.hs
# GEN_SOURCES=src/Lexer.x src/Parser.y
# GENERATED=src/Lexer.hs src/Parser.hs
PACKAGE=hw3.zip

.PHONY: pack all run clean

all: parser

run: parser
	./parser

clean:
	rm -f parser

parser: $(SOURCES)
	$(HC) -i./src -tmpdir . ./src/Main.hs -o parser

# $(GENERATED): $(GEN_SOURCES) $(SOURCES)
# 	alex src/Lexer.x -o src/Lexer.hs
# 	happy src/Parser.y -o src/Parser.hs

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src
