all: Opening Endgame selfplay clean

Opening:
	ghc Opening.hs

Endgame:
	ghc Endgame.hs

selfplay:
	ghc Selfplay.hs

clean:
	rm -f *.o *.hi

remove: clean
	rm -f Opening Endgame Selfplay
