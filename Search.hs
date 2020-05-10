module Search where

import TypeDefs
import Init
import Util
import Eval
import EvalOpening
import Debug.Trace
import Debug
import OpeningSearch
import MiddleSearch
import EndSearch

findRealBestMove :: Colour -> AllPieces -> [(Piece,Move,Float)]
totalVal c ps | getGamePoint ps == Opening = findRealBestOpeningMove c ps
              | getGamePoint ps == Middle = findRealBestOpeningMove c ps --totalMiddleVal c ps (temporary to make testing easier)
              | otherwise = findRealBestEndMove c ps

