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
                                s1 = extractPMF (findRealBestOpeningMoveWrapper d c ps l1 [])
                                s2 = extractPMF (findRealBestOpeningMoveWrapper d c ps l2 [])
                                s3 = extractPMF (findRealBestOpeningMoveWrapper d c ps r1 [])
                                s4 = extractPMF (findRealBestOpeningMoveWrapper d c ps r2 [])

-- returns the best move for one side (not sure how this handles checkmate????)
--findRealBestOpeningMove :: Int -> Colour -> AllPieces -> [(Piece, Move, Float)] -> (Piece, Move, Float)
--findRealBestOpeningMove d c ps [] = findStrongestMoveFromAll [addTrueEval (c,invertColour c) 1 d x (makeSingleBestMove x ps) | x <- makeEvalList c ps]
--findRealBestOpeningMove d c ps xs = findStrongestMoveFromAll [addTrueEval (c,invertColour c) 1 d x (makeSingleBestMove x ps) | x <- xs]

findRealBestOpeningMoveWrapper :: Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> [(Piece,Move,AllPieces, Colour, Int)] -> ((Piece, Move, Float),[(Piece,Move,AllPieces,Colour,Int)])
findRealBestOpeningMoveWrapper d c ps [] ys = findRealBestOpeningMove' d c ps (makeEvalList c ps) ys
findRealBestOpeningMoveWrapper d c ps xs ys = findRealBestOpeningMove' d c ps xs ys

findRealBestOpeningMove' :: Int -> Colour -> AllPieces -> [(Piece, Move, Float)] -> [(Piece,Move,AllPieces, Colour, Int)]-> ((Piece, Move, Float),[(Piece,Move,AllPieces,Colour,Int)])
findRealBestOpeningMove' d c ps [] ys = (((King, c, (7,4),0),(0,0),0.0),[]) --this shouldnt be necessery but for some reason it is?
findRealBestOpeningMove' d c ps xs ys | length xs == 1 = addTrueEval'' (c, invertColour c) 1 d (h,ys) (makeSingleBestMove h ps)
                                      | otherwise = (extractPMF (findStrongestMoveFromAllWithList biglist), last)
                                      where
                                          h = head xs
                                          te = addTrueEval'' (c,invertColour c) 1 d (h,ys) (makeSingleBestMove h ps)
                                          list = extractList te
                                          biglist = (te : (findRealBestOpeningMove' d c ps (tail xs) list) : [])
                                          finalsingleton = drop ((length biglist)-1) biglist
                                          last = if null finalsingleton then [] else extractList (head finalsingleton)




-- updates the evaluation for moves by looking moves into the futur2
{-
--addTrueEval' :: (Colour,Colour) -> Int -> Int -> (Piece,Move,Float) -> AllPieces -> (Piece,Move,Float)
--addTrueEval' (c,nc) l d (p,m,f) ps | l == d = if isCheckmate (invertColour c) ps then
--                                               (p,m,checkmate-(fromIntegral l))
--                                           else if isCheckmate c ps then
--                                               (p,m,0-checkmate-(fromIntegral l))
--                                           else (p,m,totalVal c ps)
                                -- | l == 0 = if f == checkmate then (p,m,f) else addTrueEval (c,(invertColour nc)) (l+1) d (p,m,0) (executeMove p m ps)
--                                | otherwise = if isCheckmate (invertColour c) ps then
--                                                  (p,m,checkmate-(fromIntegral l))
--                                              else if isCheckmate c ps then
--                                                  (p,m,0-checkmate-(fromIntegral l))
--                                              else addTrueEval (c,(invertColour nc)) (l+1) d (p,m,0) (makeSingleBestMove (findRealBestOpeningMove (d-l) nc ps []) ps)
                                              --else (p,m,getMoveEval (findRealBestOpeningMove (d-l) nc ps []))
                                  --where
                                      --e = findSingleBestMove nc ps
                                      --e = findRealBestOpeningMove2 (d-l) nc ps
                                      --e = findRealBestOpeningMove (d-l) nc ps []
                                      --v = if nc == c then (totalVal c ps) + materialInDanger (invertColour c) ps else (totalVal c ps) - materialInDanger c ps
                                      --v = if c == nc then totalVal c ps else 0 - totalVal nc ps
                                      --v = totalVal c ps

--}
{-
-- updates the evaluation for moves by looking moves into the futur2
--addTrueEval :: (Colour,Colour) -> Int -> Int -> (Piece,Move,Float) -> AllPieces -> (Piece,Move,Float)
--addTrueEval (c,nc) l d (p,m,f) ps = do if l==d then
--                                            if isCheckmate (invertColour c) ps then
--                                                (p,m,checkmate-(fromIntegral l))
--                                            else if isCheckmate c ps then
--                                                (p,m,0-checkmate+(fromIntegral l))
--                                            else
--                                                (p,m,totalVal c ps)
--                                        else
--                                            if isCheckmate (invertColour c) ps then
--                                                (p,m,checkmate-(fromIntegral l))
--                                            else if isCheckmate c ps then
--                                                (p,m,0-checkmate+(fromIntegral l))
--                                            else
--                                                addTrueEval (c,(invertColour nc)) (l+1) d (p,m,0) (makeSingleBestMove (findRealBestOpeningMove (d-l) nc ps []) ps)

-}
-- ((p,m,f),[(Piece,Move,AllPieces, Colour, l)])
-- updates the evaluation for moves by looking moves into the futur2
addTrueEval'' :: (Colour,Colour) -> Int -> Int -> ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int)]) -> AllPieces -> ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int)])
addTrueEval'' (c,nc) l d ((p,m,f),xs) ps = if l==d then
                                               if isCheckmate (invertColour c) ps then
                                                  ((p,m,checkmate-(fromIntegral l)),xs)
                                               else if isCheckmate c ps then
                                                  ((p,m,0-checkmate+(fromIntegral l)),xs)
                                               else
                                                  ((p,m,totalVal c ps),xs)
                                           else
                                               if isCheckmate (invertColour c) ps then
                                                  ((p,m,checkmate-(fromIntegral l)),xs)
                                               else if isCheckmate c ps then
                                                  ((p,m,0-checkmate+(fromIntegral l)),xs)
                                               else
                                                    if null move then
                                                        addTrueEval'' (c,(invertColour nc)) (l+1) d ((p,m,0),(np,nm,ps,nc,d-l):ys) (makeSingleBestMove (np,nm,nf) ps)
                                                    else -- if there is an existing best move already of equal or greater accuracy that would otherwise be achieved with a search
                                                          addTrueEval'' (c,(invertColour nc)) (l+1) d ((p,m,0),xs) (makeSingleBestMove (head move) ps)
                                           where
                                               move = getExistingBestMove (d-l) xs ps nc -- existing move
                                               ((np,nm,nf),ys) = findRealBestOpeningMoveWrapper (d-l) nc ps [] xs-- newmove



-- search to see if the best move for this case has already been found
getExistingBestMove :: Int -> [(Piece,Move,AllPieces, Colour, Int)] -> AllPieces -> Colour -> [(Piece,Move,Float)]
getExistingBestMove d xs ps c = [(p,m,0.0) | (p,m,board,col,l) <- xs, c==col, ps==board, l>=d]

getExistingEval :: Colour -> AllPieces -> [(AllPieces, Float)] -> Float
getExistingEval c ps xs = 0.0

extractPMF :: ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int)]) -> (Piece,Move,Float)
extractPMF ((p,m,f),_) = (p,m,f)

extractList :: ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int)]) -> [(Piece,Move,AllPieces,Colour,Int)]
extractList (_,xs) = xs

-- returns the best move which can be made without looking ahead WORKING
--findSingleBestMove :: Colour -> AllPieces -> (Piece, Move, Float)
--findSingleBestMove c ps = findStrongestMoveFromAll (makeEvalList c ps)


-----
-----
-- buildTree 1 Black (buildBranches 0 addAllPieces White)
-- addLeafEval Black (buildTree 1 Black (buildBranches 0 addAllPieces White))
-----
-----

{-
newsearchtestfunc :: AllPieces
newsearchtestfunc = findBestFirstBoard White $! (propagateEval 3 White $! (addLeafEval Black $! (buildTree 1 Black $! (buildBranches 0 addAllPieces White))))


findBestFirstBoard :: Colour -> Tree -> AllPieces
findBestFirstBoard c t | not (null xs) = trace (show zs) getCurrentBoard (head xs)
                       | otherwise = []
--                         where
--                             zs = [n | n <- t, getDepthLevel n == 0]
--                             xs = if c==White then [n | n <- t, all (\y -> (getNodeEval y) <= (getNodeEval n)) zs] else [n | n <- t, all (\y -> (getNodeEval y) >= (getNodeEval n)) zs]



-- add values to every node in a tree which already has eval for the leafs
--propagateEval :: Int -> Colour -> Tree -> Tree
--propagateEval 0 c t = trace "PROPAGATEEVAL*****" inheritVals 0 c t
--propagateEval l c t = trace ("PROPAGATEEVAL****"++(show l)) propagateEval (l-1) (invertColour c) (inheritVals l c t)

-- inherit the values for one level
--inheritVals :: Int -> Colour -> Tree -> Tree
--inheritVals d c t = trace "INHERIT VALS******"[(prev,ps,if l==d then trace "getting eval for this level" bestEval (prev,ps,e,l) c t else trace "not getting eval for this level yet" e,l) | (prev,ps,e,l) <- t]

-- find the value of the best move which can be made from a node
--bestEval :: Node -> Colour -> Tree -> Float
--bestEval node c t | not (null xs) = trace "BESTEVAL******getting best eval" getNodeEval (head xs)
--                  | otherwise = trace "BESTEVAL******just returning 0" 0.0
--                        where
--                            zs = [n | n <- t, getDepthLevel n == ((getDepthLevel node)+1), getPrevBoard n == (getCurrentBoard node)]
--                            xs = if c==White then [n | n <- zs, all (\y -> (getNodeEval y) <= (getNodeEval n)) zs ] else [n | n <- zs, all (\y -> (getNodeEval y) >= (getNodeEval n)) zs ]

-- search for a board
--searchForEval :: Int -> Tree -> AllPieces -> Float
--searchForEval l t ps | not (null zs) = getNodeEval (head zs)
--                     | otherwise = 0.0
--                     where
--                         zs = [n | n <- t, getDepthLevel n == l, getCurrentBoard n == ps, getNodeEval n /= 0.0]

-- add final eval to tree
--addLeafEval :: Colour -> Tree -> Tree
--addLeafEval c t = trace "ADDLEAFEVAL***"[(prev,ps,if l==4 then (if searchForEval l t ps /= 0.0 then searchForEval l t ps else z (totalVal c ps)) else e, l)| (prev,ps,e,l) <- t]
--                  where
                      --z = if c == Black then (\y -> 0 - y) else (\y -> y)
                      --searcher = (\y -> searchForEval l t y)
--                      xs = [n | n <- t, getDepthLevel n == 4]
--                      z = (\y -> if c==White then y else 0-y)

-- pass 1 as first l value and buildBranches 0 <board> as initial tree
--buildTree :: Int -> Colour -> Tree -> Tree
--buildTree 4 c t = trace "BUILDTREE****" addAllBranches c 4 t
--buildTree l c t = trace "BUILDTREE****" buildTree (l+1) (invertColour c) (addAllBranches c l t)

--addAllBranches :: Colour -> Int -> Tree -> Tree
--addAllBranches c l t = trace "ADDALLBRANCHES*****" t++(combineTrees [buildBranches l (getCurrentBoard n) c | n <- t, getDepthLevel n == (l-1)])

--make list of all possible new board with this board as previous board
--buildBranches :: Int -> AllPieces -> Colour -> Tree
--buildBranches l ps c = trace "BUILDBRANCHES******" [ (ps, executeMove x y ps,0,l)| x <- ps, getPos x /= (-1,-1), getColour x == c, y <- legalMoves x ps]

-- takes a list of trees and converts it to one tree with all the nodes
--combineTrees :: [Tree] -> Tree
--combineTrees [] = []
--combineTrees (x:xs) = x ++ combineTrees xs

--getNodeEval :: Node -> Float
--getNodeEval (_,_,f,_) = f

--getPrevBoard :: Node -> AllPieces
--getPrevBoard (ps,_,_,_) = ps

--getDepthLevel :: Node -> Int
--getDepthLevel (_,_,_,l) = l

--getCurrentBoard :: Node -> AllPieces
--getCurrentBoard (_,ps,_,_) = ps

--depth :: Int
--depth = 4
---}


-- returns the stronget move from a list of moves with evaluations
findStrongestMoveFromAll :: [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll xs | not (null xs) = head [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ]
                            | otherwise = ((King, White, (7,4), 0), (0,0), 0-checkmate)

findStrongestMoveFromAllWithList :: [((Piece,Move,Float),[(Piece,Move,AllPieces,Colour,Int)])] -> ((Piece,Move,Float),[(Piece,Move,AllPieces,Colour,Int)])
findStrongestMoveFromAllWithList xs | not (null xs) = head [ x | x <- xs, all (\y -> (getMoveEval (extractPMF y)) <= (getMoveEval (extractPMF x))) xs ]
                                    | otherwise = (((King, White, (7,4), 0), (0,0), 0-checkmate),[])

combineMoveBases :: [((Piece,Move,Float),[(Piece,Move,AllPieces,Colour,Int)])] -> [(Piece,Move,AllPieces,Colour,Int)]
combineMoveBases [] = []
combineMoveBases (x:xs) = removeDuplicates (extractList x ++ combineMoveBases xs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

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
