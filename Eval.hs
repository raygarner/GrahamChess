module Eval where

import           TypeDefs
import           Util
import           Debug
import           EvalOpening
import           EvalMiddle
import           EvalEnd
import           Debug.Trace

totalVal :: Colour -> AllPieces -> Float
totalVal c ps | getGamePoint ps == Opening = totalOpeningVal c ps
              | getGamePoint ps == Middle = totalOpeningVal c ps --totalMiddleVal c ps (temporary to make testing easier)
              | otherwise = totalEndVal c ps


<<<<<<< HEAD
-- returns whether there are no queens on the board
noQueens :: AllPieces -> Bool
noQueens ps = null [ x | x <- ps, getPieceType x == Queen, getPos x /= (-1,-1) ]

-- returns whether there is a low ammount of material on the board
lowMaterial :: AllPieces -> Bool
lowMaterial ps = length [ x | x <- ps, getPos x /= (-1,-1), getPieceType x /= Pawn, getPieceType x /= King ] <= 8

-- return what point the game is in
getGamePoint :: AllPieces -> GamePoint
--getGamePoint ps | (noQueens ps || lowMaterial ps) = End
getGamePoint ps | lowMaterial ps = End
                | allPiecesMoved ps = Opening
                | otherwise = Opening
=======
>>>>>>> 60785291929df4edc009493c950959392256af45
