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
--openingMoveWrapper d c ps = findStrongestMoveFromAll (par j (k:j:[]))
openingMoveWrapper d c ps = findStrongestMoveFromAll (par s4 (par s3 (par s2 (s1:s2:s3:s4:[]))))
                            where
                                a = makeEvalList c ps
                                e = length a
                                l = take ((e `div` 2)+1) a
                                r = drop (e `div` 2) a
                                l1 = take ((e `div` 4)+1) l
                                l2 = drop (e `div` 4) l
                                r1 = take ((e `div` 4)+1) r
                                r2 = drop (e `div` 4) r
                                --j = findRealBestOpeningMove d c ps l
                                --k = findRealBestOpeningMove d c ps r
                                s1 = findRealBestOpeningMove d c ps l1
                                s2 = findRealBestOpeningMove d c ps l2
                                s3 = findRealBestOpeningMove d c ps r1
                                s4 = findRealBestOpeningMove d c ps r2

-- returns the best move for one side (not sure how this handles checkmate????)
findRealBestOpeningMove :: Int -> Colour -> AllPieces -> [(Piece, Move, Float)] -> (Piece, Move, Float)
findRealBestOpeningMove d c ps [] = findStrongestMoveFromAll [addTrueEval (c,invertColour c) 1 d x (makeSingleBestMove x ps) | x <- makeEvalList c ps]
findRealBestOpeningMove d c ps xs = findStrongestMoveFromAll [addTrueEval (c,invertColour c) 1 d x (makeSingleBestMove x ps) | x <- xs]


-- updates the evaluation for moves by looking moves into the futur2
addTrueEval :: (Colour,Colour) -> Int -> Int -> (Piece,Move,Float) -> AllPieces -> (Piece,Move,Float)
addTrueEval (c,nc) l d (p,m,f) ps | l == d = if isCheckmate (invertColour c) ps then
                                               (p,m,checkmate-(fromIntegral l))
                                           else if isCheckmate c ps then
                                               (p,m,0-checkmate-(fromIntegral l))
                                           else (p,m,totalVal c ps)
                                -- | l == 0 = if f == checkmate then (p,m,f) else addTrueEval (c,(invertColour nc)) (l+1) d (p,m,0) (executeMove p m ps)
                                | otherwise = if isCheckmate (invertColour c) ps then
                                                  (p,m,checkmate-(fromIntegral l))
                                              else if isCheckmate c ps then
                                                  (p,m,0-checkmate-(fromIntegral l))
                                              else addTrueEval (c,(invertColour nc)) (l+1) d (p,m,0) (makeSingleBestMove (findRealBestOpeningMove (d-l) nc ps []) ps)
                                              --else (p,m,getMoveEval (findRealBestOpeningMove (d-l) nc ps []))
                                  --where
                                      --e = findSingleBestMove nc ps
                                      --e = findRealBestOpeningMove2 (d-l) nc ps
                                      --e = findRealBestOpeningMove (d-l) nc ps []
                                      --v = if nc == c then (totalVal c ps) + materialInDanger (invertColour c) ps else (totalVal c ps) - materialInDanger c ps
                                      --v = if c == nc then totalVal c ps else 0 - totalVal nc ps
                                      --v = totalVal c ps

-- returns the best move which can be made without looking ahead WORKING
--findSingleBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
--findSingleBestMove c ps = findStrongestMoveFromAll (makeEvalList c ps)


-- pass 1 as first l value and buildBranches 0 <board> as initial tree
buildTree :: Int -> Colour -> Tree -> Tree
buildTree 4 c t = addAllBranches c 4 t
buildTree l c t = buildTree (l+1) (invertColour c) (addAllBranches c l t)

addAllBranches :: Colour -> Int -> Tree -> Tree
addAllBranches c l t = t++(combineTrees [buildBranches l (getCurrentBoard n) c | n <- t, getDepthLevel n == (l-1)])

--make list of all possible new board with this board as previous board
buildBranches :: Int -> AllPieces -> Colour -> Tree
buildBranches l ps c = [ (ps, executeMove x y ps,0,l)| x <- ps, getPos x /= (-1,-1), getColour x == c, y <- legalMoves x ps]

-- takes a list of trees and converts it to one tree with all the nodes
combineTrees :: [Tree] -> Tree
combineTrees [] = []
combineTrees (x:xs) = x ++ combineTrees xs

getNodeEval :: Node -> Float
getNodeEval (_,_,f,_) = f

getPrevBoard :: Node -> AllPieces
getPrevBoard (ps,_,_,_) = ps

getDepthLevel :: Node -> Int
getDepthLevel (_,_,_,l) = l

getCurrentBoard :: Node -> AllPieces
getCurrentBoard (_,ps,_,_) = ps





-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll xs | not (null xs) = head [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ]
                            | otherwise = ((King, White, (7,4), 0), (0,0), 0-checkmate)

--takes the top n rated moves from evalList
--takeTopMoves :: Int -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
--takeTopMoves n [] = []
--takeTopMoves 99 xs = []
--takeTopMoves n xs = m : takeTopMoves (n+1) (removeMove m xs)
--                  where
--                      m = findStrongestMoveFromAll xs

-- removes a move from a list
--removeMove :: (Piece,Move,Float) -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
--removeMove x [y] = []
--removeMove a bs = [x | x <- bs, x /= a]

-- generates a list of all legal moves for one side with evaluations
makeEvalList :: Colour -> AllPieces -> [(Piece, Move, Float)]
--makeEvalList c ps = [ (x,y,evalMove x y ps) | x <- ps, getPos x /= (-1,-1), getColour x == c, y <- legalMoves x ps ]
makeEvalList c ps = [ (x,y,0) | x <- ps, getPos x /= (-1,-1), getColour x == c, y <- legalMoves x ps ]

-- makes a move which is stored using the format with eval
makeSingleBestMove :: (Piece, Move, Float) -> AllPieces -> AllPieces
makeSingleBestMove (a,b,_) ps = executeMove a b ps

-- makes a move and then evaluates the new AllPieces
evalMove :: Piece -> Move -> AllPieces -> Float
evalMove a m ps | isCheckmate (invertColour (getColour a)) (executeMove a m ps) = checkmate -- if this is a mating move
                | otherwise = 0 --totalVal (getColour a) (executeMove a m ps)

isCheckmate :: Colour -> AllPieces -> Bool
isCheckmate c ps = null (allLegalMoves c ps) && isKingInCheck king ps
                   where
                     king = head (findPiece (findKing c ps) ps)
--isCheckmate c ps = null [ y | x <- ps, getColour x == c, y <- legalMoves x ps, getPos x /= (-1,-1) ]

checkmate :: Float
checkmate = 10000.0
