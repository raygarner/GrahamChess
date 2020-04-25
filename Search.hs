module Search where

import TypeDefs
import Debug
import Init
import Util
import Eval

-- returns the strongest move which can be made in a position
--findBestMove :: Int -> Colour -> AllPieces -> (Piece,Move)
--findBestMove l c ps | l == 5 = findSingleBestMove c ps
--                    | otherwise findSingleBestMove (makeSingleBestMove (findBestMove (++l) (invertColour c) ps))


-- returns the best move which can be made without looking ahead
--findSingleBestMove :: Colour -> AllPieces -> (Piece, Move)



--makeSingleBestMove :: (Piece, Move) -> AllPieces -> AllPieces
--makeSingleBestMove (a,b) ps = movePiece m n ps

-- makes a move and then evaluates the new AllPieces
evalMove :: Piece -> Move -> AllPieces -> Float
evalMove a m ps = totalVal (getColour a) (movePiece a m ps)

--
