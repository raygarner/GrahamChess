module Search where

import TypeDefs
import Debug
import Init
import Util
import Eval

-- returns the strongest move which can be made in a position
--findBestMove :: Int -> Colour -> AllPieces -> (Piece,Move,Float)
--findBestMove l c ps | l == 5 = findSingleBestMove c ps
--                    | otherwise findSingleBestMove c (makeSingleBestMove (findBestMove (++l) (invertColour c) ps))


-- returns the best move which can be made without looking ahead
findSingleBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
findSingleBestMove c ps = findStrongestMoveFromAll (makeEvalList c ps)

-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll [(p,m,f)] = (p,m,f)
findStrongestMoveFromAll ((p,m,f):xs) = if f > getMoveEval (findStrongestMoveFromAll xs) then (p,m,f) else findStrongestMoveFromAll xs

getMoveEval :: (Piece, Move, Float) -> Float
getMoveEval (_,_,f) = f

makeEvalList :: Colour -> AllPieces -> [(Piece, Move, Float)]
makeEvalList c ps = [ (x,y,evalMove x y ps) | x <- ps, getColour x == c, y <- legalMoves x ps ]

makeSingleBestMove :: (Piece, Move) -> AllPieces -> AllPieces
makeSingleBestMove (a,b) ps = movePiece a b ps

-- makes a move and then evaluates the new AllPieces
evalMove :: Piece -> Move -> AllPieces -> Float
evalMove a m ps = totalVal (getColour a) (movePiece a m ps)

--
