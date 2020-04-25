module Search where

import TypeDefs
import Debug
import Init
import Util
import Eval

-- returns the best move for one side
findRealBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
findRealBestMove c ps = findStrongestMoveFromAll [ addTrueEval c 0 x ps | x <- makeEvalList c ps]

-- updates the evaluation for moves by looking moves into the futur2. l limit must be even
addTrueEval :: Colour -> Int -> (Piece,Move,Float) -> AllPieces -> (Piece,Move,Float)
addTrueEval c l (p,m,f) ps | l == 6 = (p,m, totalVal c ps)
                           | otherwise = addTrueEval (invertColour c) (l+1) (p,m,f) (makeSingleBestMove (findSingleBestMove c ps) ps)

-- returns the best move which can be made without looking ahead
findSingleBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
findSingleBestMove c ps = findStrongestMoveFromAll (makeEvalList c ps)

-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll [(p,m,f)] = (p,m,f)
findStrongestMoveFromAll ((p,m,f):x:xs) = if f > getMoveEval x then (p,m,f) else findStrongestMoveFromAll (x:xs)

-- extracts the evaluation element of the move tuple
getMoveEval :: (Piece, Move, Float) -> Float
getMoveEval (_,_,f) = f

-- generates a list of all legal moves for one side with evaluations
makeEvalList :: Colour -> AllPieces -> [(Piece, Move, Float)]
makeEvalList c ps = [ (x,y,evalMove x y ps) | x <- ps, getColour x == c, y <- legalMoves x ps ]

-- makes a move which is stored using the format with eval
makeSingleBestMove :: (Piece, Move, Float) -> AllPieces -> AllPieces
makeSingleBestMove (a,b,_) ps = movePiece a b ps

-- makes a move and then evaluates the new AllPieces
evalMove :: Piece -> Move -> AllPieces -> Float
evalMove a m ps = totalVal (getColour a) (movePiece a m ps)

--
