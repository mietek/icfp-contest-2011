all: example experimental ignorant-simple ignorant-improved

example:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/example src/example.hs

experimental:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/experimental src/experimental.hs

ignorant-simple:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/ignorant-simple src/ignorant-simple.hs

ignorant-improved:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/ignorant-improved src/ignorant-improved.hs
