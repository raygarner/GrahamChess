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
findRealBestMove c ps | getGamePoint ps == Opening = openingMoveWrapper 4 c ps
                      | getGamePoint ps == Middle = middleMoveWrapper 4 c ps --totalMiddleVal c ps (temporary to make testing easier)
                      | getGamePoint ps == End = endSearchWrapper 4 c ps -- 6
                      | otherwise = endSearchWrapper 6 c ps -- 8
