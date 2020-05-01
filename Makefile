all: main selfplay

main:
	ghc Main.hs
	rm *.o *.hi

selfplay:
	ghc Selfplay.hs
	rm *.o *.hi

clean:
	rm *.o *.hi Selfplay Main
