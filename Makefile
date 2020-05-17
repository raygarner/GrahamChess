
FLAGS=-O2

all: remove clean

Opening:
	ghc $(FLAGS) Opening.hs

Endgame:
	ghc $(FLAGS) Endgame.hs

selfplay:
	ghc $(FLAGS) Selfplay.hs

userselfplay:
	ghc $(FLAGS) UserSelfplay.hs

clean: Opening Endgame selfplay userselfplay
	rm -f *.o *.hi

remove:
	rm -f *.o *.hi
	rm -f Opening Endgame Selfplay UserSelfplay
