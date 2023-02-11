zerorack: src/*.hs
	mkdir -p build
	ghc -Wall src/*.hs -outputdir build -o zerorack
