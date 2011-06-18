all: example basic

example:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/example src/example.hs

basic:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/basic src/basic.hs
