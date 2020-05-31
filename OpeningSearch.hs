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
                                --s1 = extractPMF (findRealBestOpeningMoveWrapper d c ps l1 [])
                                --s2 = extractPMF (findRealBestOpeningMoveWrapper d c ps l2 [])
                                --s3 = extractPMF (findRealBestOpeningMoveWrapper d c ps r1 [])
                                --s4 = extractPMF (findRealBestOpeningMoveWrapper d c ps r2 [])
                                s1 = findMostEpicMove (-2000000,2000000) d c ps l1
                                s2 = findMostEpicMove (-2000000,2000000) d c ps l2
                                s3 = findMostEpicMove (-2000000,2000000) d c ps r1
                                s4 = findMostEpicMove (-2000000,2000000) d c ps r2



-- returns the best move for one side (not sure how this handles checkmate????)
--findRealBestOpeningMove :: Int -> Colour -> AllPieces -> [(Piece, Move, Float)] -> (Piece, Move, Float)
--findRealBestOpeningMove d c ps [] = findStrongestMoveFromAll [addTrueEval (c,invertColour c) 1 d x (makeSingleBestMove x ps) | x <- makeEvalList c ps]
--findRealBestOpeningMove d c ps xs = findStrongestMoveFromAll [addTrueEval (c,invertColour c) 1 d x (makeSingleBestMove x ps) | x <- xs]

-- FINDMOSTEPICMOVE
--findMostEpicMove :: Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> (Piece,Move,Float)
--findMostEpicMove 0 c ps xs = findStrongestMoveFromAll c [(p,m,totalOpeningVal (executeMove p m ps)) | (p,m,f) <- moves]
--                             where
--                                 moves = if null xs then makeEvalList c ps else xs
--findMostEpicMove d c ps xs = findStrongestMoveFromAll c [(p,m, getMoveEval (findMostEpicMove (d-1) (invertColour c) (makeSingleBestMove (p,m,f) ps) [])) | (p,m,f) <- moves]
--                             where
--                                 moves = if null xs then makeEvalList c ps else xs

---------------------------------------------------
--findMostEpicMove :: Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> (Piece,Move,Float)
--findMostEpicMove 0 c ps xs = ((King,White,(0,0),0),(0,0),totalOpeningVal ps)
--findMostEpicMove d c ps xs = findStrongestMoveFromAll c [(p,m, getMoveEval (findMostEpicMove (d-1) (invertColour c) (makeSingleBestMove (p,m,f) ps) [])) | (p,m,f) <- moves]
--findMostEpicMove d c ps xs = findStrongestMoveFromAll c (addEvals d c ps moves)
--                             where
--                                 moves = if null xs then makeEvalList c ps else xs

--addEvals :: Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> [(Piece,Move,Float)]
--addEvals d c ps [] = []
--addEvals d c ps ((p,m,f):xs) = (p,m,getMoveEval (findMostEpicMove (d-1) (invertColour c) (makeSingleBestMove (p,m,f) ps) [])) : addEvals d c ps xs

------------------------------------------------------
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

----------------------------------------------------------

--findRealBestOpeningMoveWrapper :: Int -> Colour -> AllPieces -> [(Piece,Move,Float)] -> [(Piece,Move,AllPieces, Colour, Int,Float)] -> ((Piece, Move, Float),[(Piece,Move,AllPieces,Colour,Int,Float)])
--findRealBestOpeningMoveWrapper d c ps [] ys = findRealBestOpeningMove' d c ps (makeEvalList c ps) ys
--findRealBestOpeningMoveWrapper d c ps xs ys = findRealBestOpeningMove' d c ps xs ys

--findRealBestOpeningMove' :: Int -> Colour -> AllPieces -> [(Piece, Move, Float)] -> [(Piece,Move,AllPieces, Colour, Int,Float)]-> ((Piece, Move, Float),[(Piece,Move,AllPieces,Colour,Int,Float)])
--findRealBestOpeningMove' d c ps [] ys = (((King, c, (7,4),0),(0,0),0.0),[]) --this shouldnt be necessery but for some reason it is?
--findRealBestOpeningMove' d c ps xs ys | length xs == 1 = if explore then addTrueEval'' (c, invertColour c) 1 d (h,ys) mademove else didntexplore
--findRealBestOpeningMove' d c ps xs ys | length xs == 1 = addTrueEval'' (c, invertColour c) 1 d (h,ys) mademove
--                                      | otherwise = (extractPMF (findStrongestMoveFromAllWithList c biglist), last)
--                                      where
--                                          h = head xs
--                                          mademove = makeSingleBestMove h ps
--                                          --te = if explore then addTrueEval'' (c,invertColour c) 1 d (h,ys) mademove else didntexplore
--                                          te = addTrueEval'' (c,invertColour c) 1 d (h,ys) mademove
--                                          list = extractList te
--                                          biglist = (te : (findRealBestOpeningMove' d c ps (tail xs) list) : [])
--                                          finalsingleton = drop ((length biglist)-1) biglist
--                                          last = if null finalsingleton then [] else extractList (head finalsingleton)
--                                          --explore = shouldExploreMove (extractPiece h) (extractMove h) ps
                                          --didntexplore = ((extractPiece h, extractMove h, totalVal c mademove),ys)




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
-- due to the way material evaluation works depth must be an even number
--addTrueEval'' :: (Colour,Colour) -> Int -> Int -> ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int,Float)]) -> AllPieces -> ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int,Float)])
--addTrueEval'' (c,nc) l d ((p,m,f),xs) ps = if l>=d then
--                                               if isCheckmate (invertColour c) ps then
--                                                  ((p,m,checkmate-(fromIntegral l)),xs)
--                                               else if isCheckmate c ps then
--                                                  ((p,m,0-checkmate+(fromIntegral l)),xs)
--                                               else
--                                                  ((p,m,v),xs)
--
--                                           else
--                                               if isCheckmate (invertColour c) ps then
--                                                  ((p,m,checkmate-(fromIntegral l)),xs)
--                                               else if isCheckmate c ps then
--                                                  ((p,m,0-checkmate+(fromIntegral l)),xs)
--                                              else
--                                                    if null move then
--                                                        addTrueEval'' (c,(invertColour nc)) (l+1) d ((p,m,0),(np,nm,ps,nc,d-l,0.0):ys) (makeSingleBestMove (np,nm,nf) ps)
--                                                    else -- if there is an existing best move already of equal or greater accuracy that would otherwise be achieved with a search
--                                                       addTrueEval'' (c,(invertColour nc)) (l+1) d ((p,m,0),xs) (makeSingleBestMove (head move) ps)
--                                           where
--                                               move = getExistingBestMove (d-l) xs ps nc -- existing move
--                                               neweval = totalVal c ps
--                                               ((np,nm,nf),ys) = findRealBestOpeningMoveWrapper (d-l) nc ps [] xs-- newmove
--                                               v = if c==White then totalVal c ps else 0 - totalVal c ps

--shouldExploreMove :: Piece -> Move -> AllPieces -> Bool
--shouldExploreMove p m ps = isThreat p m ps || isCapture p m ps

-- returns whether a move means the piece is threatening to capture afterwards
--isThreat :: Piece -> Move -> AllPieces -> Bool
--isThreat p m ps = if (length (trulyThreatening p ps) < length (trulyThreatening newp newb)) then True else False
--isThreat p m ps = if totalMaterial c ps > totalMaterial c newb then True else False
--                  where
--                      newb = executeMove p m ps
--                      newp = head (findPiece (getTarget (getPos p) m) newb)
--                      c = invertColour (getColour p)

-- returns whether a move is a capture
--isCapture :: Piece -> Move -> AllPieces -> Bool
--isCapture p m ps = if (totalMaterial enemyCol ps) > (totalMaterial enemyCol newb) then True else False
--isCapture p m ps = if not (isEmpty t ps) then True else False
--                   where
                       --enemyCol = invertColour (getColour p)
                       --newb = executeMove p m ps
--                       pos = getPos p
--                       t = getTarget pos m

-- returns whether a move puts the enemy king in check
--isCheck :: Piece -> Move -> AllPieces -> Bool
--isCheck p m ps = False

--isPin :: Piece -> Move -> AllPieces -> Bool
--isPin p m ps = False

-- returns all pieces which a piece is threatening which it would make sense to take
--trulyThreatening :: Piece -> AllPieces -> [Piece]
--trulyThreatening p ps = [x | x <- threatening p ps, pieceMaterial x ps == 0]



-- search to see if the best move for this case has already been found
--getExistingBestMove :: Int -> [(Piece,Move,AllPieces, Colour, Int,Float)] -> AllPieces -> Colour -> [(Piece,Move,Float)]
--getExistingBestMove d xs ps c = [(p,m,0.0) | (p,m,board,col,l,f) <- xs,  ps==board, c==col, l>=d ,m /=(0,0)]


--getExistingBestMove :: Int -> [(Piece,Move,AllPieces, Colour, Int,Float)] -> AllPieces -> Colour -> [(Piece,Move,Float)]
--getExistingBestMove d [] ps c = []
--getExistingBestMove d ((p,m,board,col,l,f):xs) ps c = if ps==board && c==col && l>=d then
--                                                          [(p,m,0.0)]
--                                                      else
--                                                          getExistingBestMove d xs ps c

--getExistingEval :: AllPieces -> Colour -> [(Piece,Move,AllPieces,Colour,Int,Float)] -> [Float]
--getExistingEval ps c xs = [f | (p,m,board,col,l,f) <- xs, ps==board, c==col]

--getExistingEval :: AllPieces -> Colour -> [(Piece,Move,AllPieces,Colour,Int,Float)] -> [Float]
--getExistingEval ps c [] = []
--getExistingEval ps c ((p,m,board,col,d,f):xs) = if col==c && board==ps then
--                                                    [f]
--                                                else
--                                                    getExistingEval ps c xs


--extractPMF :: ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int,Float)]) -> (Piece,Move,Float)
--extractPMF ((p,m,f),_) = (p,m,f)

--extractList :: ((Piece,Move,Float),[(Piece,Move,AllPieces, Colour, Int,Float)]) -> [(Piece,Move,AllPieces,Colour,Int,Float)]
--extractList (_,xs) = xs

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
--newsearchtestfunc :: AllPieces
--newsearchtestfunc = findBestFirstBoard White $! (propagateEval 3 White $! (addLeafEval Black $! (buildTree 1 Black $! (buildBranches 0 addAllPieces White))))


--findBestFirstBoard :: Colour -> Tree -> AllPieces
--findBestFirstBoard c t | not (null xs) = trace (show zs) getCurrentBoard (head xs)
--                       | otherwise = []
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
findStrongestMoveFromAll :: Colour -> [(Piece,Move,Float)] -> (Piece,Move,Float)
findStrongestMoveFromAll c xs | not (null xs) = head list
                              | otherwise = ((King, White, (7,4), 0), (0,0), 0-checkmate)
                                where
                                    list = if c==White then [ x | x <- xs, all (\y -> (getMoveEval y) <= (getMoveEval x)) xs ] else [ x | x <- xs, all (\y -> (getMoveEval y) >= (getMoveEval x)) xs ]



--findStrongestMoveFromAllWithList :: Colour -> [((Piece,Move,Float),[(Piece,Move,AllPieces,Colour,Int,Float)])] -> ((Piece,Move,Float),[(Piece,Move,AllPieces,Colour,Int,Float)])
--findStrongestMoveFromAllWithList c xs | not (null xs) = head list
--                                      | otherwise = (((King, White, (7,4), 0), (0,0), 0-checkmate),[])
--                                        where
--                                            list = if c==White then [ x | x <- xs, all (\y -> (getMoveEval (extractPMF y)) <= (getMoveEval (extractPMF x))) xs ] else [ x | x <- xs, all (\y -> (getMoveEval (extractPMF y)) >= (getMoveEval (extractPMF x))) xs ]


--combineMoveBases :: [((Piece,Move,Float),[(Piece,Move,AllPieces,Colour,Int,Float)])] -> [(Piece,Move,AllPieces,Colour,Int,Float)]
--combineMoveBases [] = []
--combineMoveBases (x:xs) = removeDuplicates (extractList x ++ combineMoveBases xs)

--removeDuplicates :: Eq a => [a] -> [a]
--removeDuplicates = rdHelper []
--    where rdHelper seen [] = seen
--          rdHelper seen (x:xs)
--              | x `elem` seen = rdHelper seen xs
--              | otherwise = rdHelper (seen ++ [x]) xs

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
--evalMove :: Piece -> Move -> AllPieces -> Float
--evalMove a m ps | isCheckmate (invertColour (getColour a)) (executeMove a m ps) = checkmate -- if this is a mating move
--                | otherwise = 0 --totalVal (getColour a) (executeMove a m ps)

isCheckmate :: Colour -> AllPieces -> Bool
isCheckmate c ps = null (allLegalMoves c ps) && isKingInCheck king ps
                   where
                     king = head (findPiece (findKing c ps) ps)
--isCheckmate c ps = null [ y | x <- ps, getColour x == c, y <- legalMoves x ps, getPos x /= (-1,-1) ]

checkmate :: Float
checkmate = 10000.0
