zerorack: src/*.hs
	mkdir -p build
	ghc -Wall src/Main.hs -outputdir build -o zerorack
