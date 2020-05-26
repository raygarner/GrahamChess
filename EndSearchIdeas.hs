module EndSearchIdeas where

import TypeDefs
import Init
import Util
import Eval
--import EvalOpening
import Debug.Trace
import Debug
import Control.Parallel



-- findBestMove :: Colour -> AllPieces -> (Piece,Move,Float)
-- findBestMove c ps = getStrongestMoveFromList (getGoodMoveScores c (getFavouriteMoves moves) ++ getBadMoveScores c (getBadMoves moves ps))
--                         where
--                           moves = makeEvalList c ps




getBadMoveScores :: Colour -> [(Piece,Move,Float)] -> AllPieces -> [(Piece,Move,Float)]
getBadMoveScores c xs ps = [addTrueEval (c,c) 0 4 x ps | x <- xs]

getGoodMoveScores :: Colour -> [(Piece,Move,Float)] -> AllPieces -> [(Piece,Move,Float)]
getGoodMoveScores c xs ps = [addTrueEval (c,c) 0 2 x ps | x <- xs]

-- updates the evaluation for moves by looking moves into the futur2
addTrueEval :: (Colour,Colour) -> Int -> Int -> (Piece,Move,Float) -> AllPieces -> (Piece,Move,Float)
addTrueEval (c,nc) l d (p,m,f) ps | l == d = if isCheckmate (invertColour c) ps then
                                               (p,m,checkmate-(fromIntegral l))
                                           else if isCheckmate c ps then
                                               (p,m,0-checkmate-(fromIntegral l))
                                           else  (p,m,v)
                                | l == 0 = if f == checkmate then (p,m,f) else addTrueEval (c,(invertColour nc)) (l+1) d (p,m,0) (executeMove p m ps)
                                | otherwise = if isCheckmate (invertColour c) ps then
                                                  (p,m,checkmate-(fromIntegral l))
                                              else if isCheckmate c ps then
                                                  (p,m,0-checkmate-(fromIntegral l))
                                              else
                                                addTrueEval (c,(invertColour nc)) (l+1) d (p,m,0) (makeSingleBestMove e ps)
                                  where
                                      e = findBestDeepMove (d-l) nc ps
                                      v = totalVal c ps


findBestDeepMove :: Int -> Colour -> AllPieces -> (Piece,Move,Float)
findBestDeepMove d c ps = findStrongestMoveFromAll [addTrueEval (c,c) 0 d x ps | x <- getFavouriteMoves (makeEvalList c ps)]











getFavouriteMoves :: [(Piece,Move,Float)] -> [(Piece,Move,Float)]
getFavouriteMoves pieces = takeTopMoves 0 e pieces
                           where
                             e = div (length pieces) 2


getBadMoves :: [(Piece,Move,Float)] -> [(Piece,Move,Float)]
getBadMoves pieces = takeBottomMoves 0 e pieces
              where
                e = div (length pieces) 4

-- takes the bottom n rated moves from a evalist
takeBottomMoves :: Int -> Int -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
takeBottomMoves i n xs | null xs = []
                       | i == n = []
                       | otherwise = m : takeBottomMoves (i + 1) n (removeMove m xs)
                       where
                         m = findWorstMoveFromAll xs


-- returns the weakest move from a list of moves with evaluation
findWorstMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findWorstMoveFromAll xs | not (null xs) = head [x | x <- xs, all (\y -> (getMoveEval y) >= (getMoveEval x)) xs]

-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll xs | not (null xs) = head [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ]
                            | otherwise = ((King, White, (7,4), 0), (0,0), 0-checkmate)


--takes the top n rated moves from evalList
takeTopMoves :: Int -> Int -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
takeTopMoves i n xs | null xs = []
                    | i == n = []
                    | otherwise = m : takeTopMoves (i + 1) n (removeMove m xs)
                    where
                      m = findStrongestMoveFromAll xs


-- makes a move which is stored using the format with eval
makeSingleBestMove :: (Piece, Move, Float) -> AllPieces -> AllPieces
makeSingleBestMove (a,b,_) ps = executeMove a b ps

-- removes a move from a list
removeMove :: (Piece,Move,Float) -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
removeMove x [y] = []
removeMove a bs = [x | x <- bs, x /= a]

-- generates a list of all legal moves for one side with evaluations
makeEvalList :: Colour -> AllPieces -> [(Piece, Move, Float)]
makeEvalList c ps = [ (x,y,evalMove x y ps) | x <- ps, getColour x == c, y <- legalMoves x ps, getPos x /= (-1,-1) ]

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

futureCheckmate :: Float
futureCheckmate = 250.0
