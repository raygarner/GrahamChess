module EndSearch where

import TypeDefs
import Init
import Util
import Eval
--import EvalOpening
import Debug.Trace
import Debug
import Control.Parallel
import Data.List



endSearchWrapper :: Colour -> AllPieces -> (Piece,Move,Float)
endSearchWrapper c ps = findStrongestMoveFromAll (par s2 (s1:s2:[]))
                            where
                                a = makeEvalList c ps
                                e = length a
                                l = take ((e `div` 2)+1) a
                                r = drop (e `div` 2) a
                                s1 = findBestMove c ps r
                                s2 = findBestMove c ps l



findBestMove :: Colour -> AllPieces -> [(Piece,Move,Float)] -> (Piece,Move,Float)
findBestMove c ps xs = findStrongestMoveFromAll (getGoodMoveScores c (getFavouriteMoves xs) ps ++ getBadMoveScores c (getBadMoves xs) ps)


getBadMoveScores :: Colour -> [(Piece,Move,Float)] -> AllPieces -> [(Piece,Move,Float)]
getBadMoveScores c xs ps = [addTrueEval (c,c) 0 6 x ps | x <- xs]

getGoodMoveScores :: Colour -> [(Piece,Move,Float)] -> AllPieces -> [(Piece,Move,Float)]
getGoodMoveScores c xs ps = [addTrueEval (c,c) 0 4 x ps | x <- xs]

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



movesWithSameScoreOrHigher :: Float -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
movesWithSameScoreOrHigher f xs = [ x | x <- xs,  getScore x >= f]

movesWithLowerScore :: Float -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
movesWithLowerScore f xs = [ x | x <- xs, getScore x < f]

getScore :: (Piece,Move,Float) -> Float
getScore (_,_,f) = f

medianValue :: [(Piece,Move,Float)] -> Float
medianValue xs = (sort [getScore x | x <- xs]) !! (div (length xs) 2)



getFavouriteMoves :: [(Piece,Move,Float)] -> [(Piece,Move,Float)]
getFavouriteMoves pieces = takeTopMoves pieces

getBadMoves :: [(Piece,Move,Float)] -> [(Piece,Move,Float)]
getBadMoves pieces = takeBottomMoves pieces

-- takes the bottom n rated moves from a evalist
takeBottomMoves :: [(Piece,Move,Float)] -> [(Piece,Move,Float)]
takeBottomMoves xs | null xs = []
                   | otherwise = take n (reverse m)
                   where
                     m = movesWithLowerScore (medianValue xs) xs
                     n = div (length m) 4


-- returns the weakest move from a list of moves with evaluation
findWorstMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findWorstMoveFromAll xs | not (null xs) = head [x | x <- xs, all (\y -> (getMoveEval y) >= (getMoveEval x)) xs]

-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll xs | not (null xs) = head [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ]
                            | otherwise = ((King, White, (7,4), 0), (0,0), 0-checkmate)


--takes the top n rated moves from evalList
takeTopMoves :: [(Piece,Move,Float)] -> [(Piece,Move,Float)]
takeTopMoves xs | null xs = []
                | otherwise = movesWithSameScoreOrHigher (medianValue xs) xs


-- makes a move which is stored using the format with eval
makeSingleBestMove :: (Piece, Move, Float) -> AllPieces -> AllPieces
makeSingleBestMove (a,b,_) ps = executeMove a b ps

removeMove :: (Piece,Move,Float) -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
removeMove x [y] = []
removeMove x ys = [a | a <- ys, a /= x]

-- removes moves from a list
removeMoves :: [(Piece,Move,Float)] -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
removeMoves x [y] = []
removeMoves [] ys = ys
removeMoves (x:xs) bs = removeMoves xs (removeMove x bs)

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
