
FLAGS=-O2 -optc-O3 -funfolding-use-threshold=16 -fexcess-precision -optc-ffast-math -threaded

FLAGS=-O2 -funfolding-use-threshold=16 -fexcess-precision -optc-ffast-math -threaded

FLAGS=-O2 -threaded

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
	make clean
	rm -f Opening Endgame Selfplay UserSelfplay
