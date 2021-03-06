
#FLAGS=-O2 -funfolding-use-threshold=16 -fexcess-precision -optc-O3 -optc-ffast-math

#FLAGS=-threaded -O2 -fexcess-precision -optc-ffast-math

#FLAGS=-threaded -O2 -fexcess-precision -optc-O3 -optc-ffast-math -fforce-recomp

#FLAGS=-threaded -O2 -fexcess-precision -optc-O3 -optc-ffast-math -fforce-recomp

FLAGS=-threaded -O2

all: remove clean

allmain:
	ghc $(FLAGS) AllMain.hs
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
