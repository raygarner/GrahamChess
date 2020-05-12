all: remove Opening Endgame selfplay userselfplay clean

Opening:
	ghc Opening.hs

Endgame:
	ghc Endgame.hs

selfplay:
	ghc Selfplay.hs

userselfplay:
	ghc UserSelfplay.hs

clean:
	rm -f *.o *.hi

remove: clean
	rm -f Opening Endgame Selfplay UserSelfplay
