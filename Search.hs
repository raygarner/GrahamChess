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

findRealBestMove :: Colour -> AllPieces -> (Piece,Move,Float)
findRealBestMove c ps | getGamePoint ps == Opening = trace "open findRealBestMove called" findRealBestOpeningMove 6 c ps
                      | getGamePoint ps == Middle = findRealBestOpeningMove 6 c ps --totalMiddleVal c ps (temporary to make testing easier)
                      | otherwise = findRealBestEndMove c ps

