all: remove clean

Opening:
	ghc Opening.hs

Endgame:
	ghc Endgame.hs

selfplay:
	ghc Selfplay.hs

userselfplay:
	ghc UserSelfplay.hs

clean: Opening Endgame selfplay userselfplay
	rm -f *.o *.hi

remove:
	rm -f *.o *.hi
	rm -f Opening Endgame Selfplay UserSelfplay
