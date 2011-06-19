all: example ignorant-example ignorant-simple aware-example aware-simple aware-q

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

aware-simple:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/aware-simple src/aware-simple.hs

aware-q:
	mkdir -p bin
	ghc --make -Wall -O2 -isrc -o bin/aware-q src/aware-q.hs

package:
	find src -name 'dist' | xargs rm -r
	find src -name '*.hi' | xargs rm
	find src -name '*.o' | xargs rm
	tar zcvf leastfixed.tar.gz README install run Makefile src
