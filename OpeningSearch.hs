module OpeningSearch where

import TypeDefs
import Init
import Util
import Eval
import EvalOpening
import Debug.Trace
import Debug

-- returns the best move for one side (not sure how this handles checkmate????)
findRealBestOpeningMove :: Colour -> AllPieces -> (Piece, Move, Float)
findRealBestOpeningMove c ps = findStrongestMoveFromAll [ addTrueEval (c,c) 0 x ps | x <- takeTopMoves 0 (makeEvalList c ps)]

getScores :: Colour -> AllPieces -> [(Piece,Move,Float)]
getScores c ps = [ addTrueEval (c,c) 0 x ps | x <- makeEvalList c ps]

-- updates the evaluation for moves by looking moves into the futur2
addTrueEval :: (Colour,Colour) -> Int -> (Piece,Move,Float) -> AllPieces -> (Piece,Move,Float)
addTrueEval (c,nc) l (p,m,f) ps | l == 24 = if isCheckmate (invertColour c) ps then
                                               (p,m,checkmate-(fromIntegral l))
                                           else if isCheckmate c ps then
                                               (p,m,0-checkmate-(fromIntegral l))
                                           else (p,m,v+f)
                                | l == 0 = if f == checkmate then (p,m,f) else addTrueEval (c,(invertColour nc)) (l+1) (p,m,v) (executeMove p m ps)
                                | otherwise = if isCheckmate (invertColour c) ps then
                                                  (p,m,checkmate-(fromIntegral l))
                                              else if isCheckmate c ps then
                                                  (p,m,0-checkmate-(fromIntegral l))
                                              else addTrueEval (c,(invertColour nc)) (l+1) (p,m,v+f) (makeSingleBestMove e ps)
                                  where
                                      e = findSingleBestMove nc ps
                                      --v = if nc == c then (totalVal c ps) + materialInDanger (invertColour c) ps else (totalVal c ps) - materialInDanger c ps
                                      v = if c == nc then totalVal c ps else 0 - totalVal nc ps
                                      --v = totalVal c ps

-- returns the total val difference
totalValDiff :: Colour -> AllPieces -> Float
totalValDiff c ps = (totalVal c ps) - (totalVal (invertColour c) ps)

-- returns the best move which can be made without looking ahead WORKING
findSingleBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
findSingleBestMove c ps = findStrongestMoveFromAll (makeEvalList c ps)

-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll xs | not (null xs) = head [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ]
                            | otherwise = ((King, White, (7,4), 0), (0,0), 0-checkmate)

--takes the top n rated moves from evalList
takeTopMoves :: Int -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
takeTopMoves n [] = []
takeTopMoves 4 xs = []
takeTopMoves n xs = m : takeTopMoves (n+1) (removeMove m xs)
                  where
                      m = findStrongestMoveFromAll xs

-- removes a move from a list
removeMove :: (Piece,Move,Float) -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
removeMove x [y] = []
removeMove a bs = [x | x <- bs, x /= a]

-- generates a list of all legal moves for one side with evaluations
makeEvalList :: Colour -> AllPieces -> [(Piece, Move, Float)]
makeEvalList c ps = [ (x,y,evalMove x y ps) | x <- ps, getColour x == c, y <- legalMoves x ps, getPos x /= (-1,-1) ]

-- makes a move which is stored using the format with eval
makeSingleBestMove :: (Piece, Move, Float) -> AllPieces -> AllPieces
makeSingleBestMove (a,b,_) ps = executeMove a b ps

-- makes a move and then evaluates the new AllPieces
evalMove :: Piece -> Move -> AllPieces -> Float
evalMove a m ps | isCheckmate (invertColour (getColour a)) (executeMove a m ps) = checkmate -- if this is a mating move
                | otherwise = totalVal (getColour a) (executeMove a m ps)

isCheckmate :: Colour -> AllPieces -> Bool
isCheckmate c ps = null (makeEvalList c ps) && isKingInCheck king ps
                   where
                     king = head (findPiece (findKing c ps) ps)
--isCheckmate c ps = null [ y | x <- ps, getColour x == c, y <- legalMoves x ps, getPos x /= (-1,-1) ]

checkmate :: Float
checkmate = 10000.0
