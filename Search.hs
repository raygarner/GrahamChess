module Search where

import TypeDefs
import Debug
import Init
import Util
import Eval

-- returns the best move for one side (not sure how this handles checkmate????)
findRealBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
findRealBestMove c ps = findStrongestMoveFromAll [ addTrueEval (c,c) 0 x ps | x <- makeEvalList c ps]

-- updates the evaluation for moves by looking moves into the futur2
addTrueEval :: (Colour,Colour) -> Int -> (Piece,Move,Float) -> AllPieces -> (Piece,Move,Float)
addTrueEval (c,nc) l (p,m,f) ps | l == 3 = (p,m, (totalVal c ps) + f)
                                | l == 0 = addTrueEval (c,(invertColour nc)) (l+1) (p,m,f) (movePiece p m ps)
                                | otherwise = addTrueEval (c,(invertColour nc)) (l+1) (p,m,f+(totalVal c ps)) (makeSingleBestMove e ps)
                                  where
                                      e = findSingleBestMove nc ps

-- returns the best move which can be made without looking ahead WORKING
findSingleBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
findSingleBestMove c ps = findStrongestMoveFromAll (makeEvalList c ps)

-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll xs = head [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ]

-- extracts the evaluation element of the move tuple
getMoveEval :: (Piece, Move, Float) -> Float
getMoveEval (_,_,f) = f

extractMove :: (Piece, Move, Float) -> Move
extractMove (_,m,_) = m

extractPiece :: (Piece, Move, Float) -> Piece
extractPiece (p,_,_) = p

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
