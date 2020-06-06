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
findMostEpicMove (a,b) 0 c ps xs = ((King,White,(0,0),0),(0,0),fromIntegral (totalOpeningVal ps))
findMostEpicMove (a,b) d c ps xs = findStrongestMoveFromAll c (addEvals (a,b) d c ps moves)
                             where
                                 moves = if null xs then makeEvalList c ps else xs


--findMostEpicMove :: (Float,Float) -> Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> (Piece,Move,Float)
--findMostEpicMove (a,b) 0 c ps xs = ((King,White,(0,0),0),(0,0),fromIntegral (totalOpeningVal ps))
--findMostEpicMove (a,b) d c ps xs | isCheckmate White ps = ((King,White,(0,0),0),(0,0), (checkmate White))
--                                 | isCheckmate Black ps = ((King,White,(0,0),0), (0,0), (checkmate Black))
--                                 | otherwise = findStrongestMoveFromAll c (addEvals (a,b) d c ps moves)
--                                             where
--                                               moves = if null xs then makeEvalList c ps else xs



addEvals :: (Float,Float) -> Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
addEvals (a,b) d c ps [] = []
addEvals (a,b) d c ps ((p,m,f):xs) = if noex then [(p,m,shorteval)] else (p,m,eval) : next
--addEvals (a,b) d c ps ((p,m,f):xs) = if (not (isCapture p m ps) && d > 3) then (p,m,fromIntegral (totalOpeningVal (executeMove p m ps))) : next else if noex then [(p,m,shorteval)] else (p,m,eval) : next
                                     where
                                         eval = getMoveEval (findMostEpicMove (a,b) (d-1) (invertColour c) (executeMove p m ps) [])
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
                              | otherwise = ((King, White, (7,4), 0), (0,0), checkmate c) -- if stalemate
                                where
                                    list = if c==White then [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ] else [ x | x <- xs, all (\y -> (getMoveEval y) >= (getMoveEval x)) xs ]


-- generates a list of all legal moves for one side with evaluations
makeEvalList :: Colour -> AllPieces -> [(Piece, Move, Float)]
--makeEvalList c ps = [ (x,y,fromIntegral (totalOpeningVal (executeMove x y ps))) | x <- ps, getPos x /= (-1,-1), getColour x == c, y <- legalMoves x ps ]
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

--takes the top n rated moves from evalList
takeTopMoves :: Colour -> Int -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
takeTopMoves c n [] = []
takeTopMoves c 0 xs = []
takeTopMoves c n xs = m : takeTopMoves c (n-1) (removeMove m xs)
                  where
                      m = findStrongestMoveFromAll c xs

-- removes a move from a list
removeMove :: (Piece,Move,Float) -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
removeMove x [y] = []
removeMove a bs = [x | x <- bs, x /= a]

-- returns whether a move means the piece is threatening to capture afterwards
isThreat :: Piece -> Move -> AllPieces -> Bool
isThreat p m ps = if (length (trulyThreatening p ps) < length (trulyThreatening newp newb)) then True else False
                  where
                      newb = executeMove p m ps
                      newp = head (findPiece (getTarget (getPos p) m) newb)

-- returns whether a move is a capture
isCapture :: Piece -> Move -> AllPieces -> Bool
isCapture p m ps = if (totalMaterial enemyCol ps) > (totalMaterial enemyCol newb) then True else False
                   where
                       enemyCol = invertColour (getColour p)
                       newb = executeMove p m ps


trulyThreatening :: Piece -> AllPieces -> [Piece]
trulyThreatening p ps = [x | x <- threatening p ps, pieceMaterial x ps == 0]
