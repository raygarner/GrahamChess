all:
	make clean
	ghc Main.hs

clean:
	rm *.o *.hi
