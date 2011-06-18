all: example experimental simple

example:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/example src/example.hs src/common.hs

experimental:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/experimental src/experimental.hs src/common.hs

simple:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/simple src/simple.hs src/common.hs
