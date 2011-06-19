all: example ignorant-example ignorant-simple aware-example

example:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/example src/example.hs

ignorant-example:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/ignorant-example src/ignorant-example.hs

ignorant-simple:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/ignorant-simple src/ignorant-simple.hs

aware-example:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/aware-example src/aware-example.hs
