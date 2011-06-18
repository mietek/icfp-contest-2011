all: example experimental ignorant-simple

example:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/example src/example.hs src/common.hs

experimental:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/experimental src/experimental.hs src/common.hs

ignorant-simple:
	mkdir -p bin
	ghc --make -Wall -O2 -o bin/ignorant-simple src/ignorant-simple.hs src/ignorant-common.hs src/common.hs
