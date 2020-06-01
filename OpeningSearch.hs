module OpeningSearch where

import TypeDefs
import Init
import Util
import Eval
import EvalOpening
--import EvalOpening
import Debug.Trace
import Debug
import Control.Parallel

openingMoveWrapper :: Int -> Colour -> AllPieces -> (Piece, Move, Float)
openingMoveWrapper d c ps = findStrongestMoveFromAll c (par s4 (par s3 (par s2 (s1:s2:s3:s4:[]))))
                            where
                                a = makeEvalList c ps
                                e = length a
                                l = take ((e `div` 2)) a
                                r = drop (e `div` 2) a
                                l1 = take ((e `div` 4)) l
                                l2 = drop (e `div` 4) l
                                r1 = take ((e `div` 4)) r
                                r2 = drop (e `div` 4) r
                                s1 = findMostEpicMove (-2000000,2000000) d c ps l1
                                s2 = findMostEpicMove (-2000000,2000000) d c ps l2
                                s3 = findMostEpicMove (-2000000,2000000) d c ps r1
                                s4 = findMostEpicMove (-2000000,2000000) d c ps r2


findMostEpicMove :: (Float,Float) -> Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> (Piece,Move,Float)
findMostEpicMove (a,b) 0 c ps xs = ((King,White,(0,0),0),(0,0),totalOpeningVal ps)
findMostEpicMove (a,b) d c ps xs = findStrongestMoveFromAll c (addEvals (a,b) d c ps moves)
                             where
                                 moves = if null xs then makeEvalList c ps else xs

addEvals :: (Float,Float) -> Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
addEvals (a,b) d c ps [] = []
addEvals (a,b) d c ps ((p,m,f):xs) = if noex then [(p,m,shorteval)] else (p,m,eval) : next
                                     where
                                         eval = getMoveEval (findMostEpicMove (a,b) (d-1) (invertColour c) (makeSingleBestMove (p,m,f) ps) [])
                                         (noex,shorteval) = dontExplore (a,b) c eval
                                         next = addEvals (updateAB (a,b) c eval) d c ps xs

dontExplore :: (Float,Float) -> Colour -> Float -> (Bool,Float)
dontExplore (a,b) c f = if c==White then
                           if f>=b then
                               (True,b)
                           else
                               (False,b)
                       else
                           if f<=a then
                               (True,a)
                           else
                               (False,a)

updateAB :: (Float,Float) -> Colour -> Float -> (Float,Float)
updateAB (a,b) c f = if c==White then
                         if f>a then
                             (f,b)
                         else
                             (a,b)
                     else
                         if f<b then
                             (a,f)
                         else
                             (a,b)

-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: Colour -> [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll c xs | not (null xs) = head list
                              | otherwise = ((King, White, (7,4), 0), (0,0), checkmate c)
                                where
                                    list = if c==White then [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ] else [ x | x <- xs, all (\y -> (getMoveEval y) >= (getMoveEval x)) xs ]


-- generates a list of all legal moves for one side with evaluations
makeEvalList :: Colour -> AllPieces -> [(Piece, Move, Float)]
makeEvalList c ps = [ (x,y,0) | x <- ps, getPos x /= (-1,-1), getColour x == c, y <- legalMoves x ps ]

-- makes a move which is stored using the format with eval
makeSingleBestMove :: (Piece, Move, Float) -> AllPieces -> AllPieces
makeSingleBestMove (a,b,_) ps = executeMove a b ps

isCheckmate :: Colour -> AllPieces -> Bool
isCheckmate c ps = null (allLegalMoves c ps) && isKingInCheck king ps
                   where
                     king = head (findPiece (findKing c ps) ps)
--isCheckmate c ps = null [ y | x <- ps, getColour x == c, y <- legalMoves x ps, getPos x /= (-1,-1) ]

checkmate :: Colour -> Float
checkmate c = if c==White then -1000000 else 1000000
