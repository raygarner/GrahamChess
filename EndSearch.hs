module EndSearch where

import TypeDefs
import Init
import Util
import Eval
--import EvalOpening
import Debug.Trace
import Debug
import Control.Parallel


endMoveWrapper :: Colour -> AllPieces -> (Piece, Move, Float)
endMoveWrapper c ps = ((Pawn,White,(0,5),0),(1,0),0.5)
