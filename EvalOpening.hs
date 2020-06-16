module EvalOpening where

import           TypeDefs
import           Util
import           Debug
import           Debug.Trace
import           Control.Parallel

-- some crude evaluations

evalPawn :: Colour -> Pos -> Int
evalPawn c (m,n) = 0 -- relboard !! i
               where
                   i = (m * 8) + n
                   board = [ 0,  0,  0,  0,  0,  0,  0,  0,
                            50, 50, 50, 50, 50, 50, 50, 50,
                            10, 10, 20, 30, 30, 20, 10, 10,
                             5,  5, 10, 20, 20, 10,  5,  5,
                             0,  0,  0, 20, 20,-25,-20,  0,
                             5, -5,-10,  0,  0,-10, -5,  5,
                             5, 10, 10,-20,-20, 10, 10,  5,
                             0,  0,  0,  0,  0,  0,  0,  0]
                   relboard = if c==White then board else reverse board


evalKnight :: Colour -> Pos -> Int
evalKnight c (m,n) = relboard !! i
                     where
                         i = (m * 8) + n
                         board = [-50,-40,-30,-30,-30,-30,-40,-50,
                                -40,-20,  0,  0,  0,  0,-20,-40,
                                -30,  0, 10, 15, 15, 10,  0,-30,
                                -30,  5, 15, 10, 10, 15,  5,-30,
                                -30,  0, 15, 20, 20, 15,  0,-30,
                                -30,  5, 10, 15, 15, 10,  5,-30,
                                -40,-20,  0,  5,  5,  0,-20,-40,
                                -50,-40,-30,-30,-30,-30,-40,-50]
                         relboard = if c==White then board else reverse board



evalBishop :: Colour -> Pos -> Int
evalBishop c (m,n) = relboard !! i
                     where
                         i = (m * 8) + n
                         board = [-20,-10,-10,-10,-10,-10,-10,-20,
                                -10,  0,  0,  0,  0,  0,  0,-10,
                                -10,  0,  5, 10, 10,  5,  0,-10,
                                -10,  5,  5, 10, 10,  5,  5,-10,
                                -10,  0, 10, 10, 10, 10,  0,-10,
                                -10, 10, 10, 10, 10, 10, 10,-10,
                                -10,  5,  0,  0,  0,  0,  5,-10,
                                -20,-10,-10,-10,-10,-10,-10,-20]
                         relboard = if c==White then board else reverse board

evalRook :: Colour -> Pos -> Int
evalRook c (m,n) = relboard !! i
                     where
                         i = (m * 8) + n
                         board = [ 0,  0,  0,  0,  0,  0,  0,  0,
                                  5, 10, 10, 10, 10, 10, 10,  5,
                                 -5,  0,  0,  0,  0,  0,  0, -5,
                                 -5,  0,  0,  0,  0,  0,  0, -5,
                                 -5,  0,  0,  0,  0,  0,  0, -5,
                                 -5,  0,  0,  0,  0,  0,  0, -5,
                                 -5,  0,  0,  0,  0,  0,  0, -5,
                                  0,  0,  0,  5,  5,  0,  0,  0]
                         relboard = if c==White then board else reverse board

evalQueen :: Colour -> Pos -> Int
evalQueen c (m,n) = (relboard !! i)
                     where
                         i = (m * 8) + n
                         board = [-20,-10,-10, -5, -5,-10,-10,-20,
                                -10, -5, -5, -5, -5, -5, -5,-10,
                                -10, -5,  -5,  -5,  -5,  -5, -5,-10,
                                 -5, -5,  -5,  -5,  -5,  -5, -5, -5,
                                 -5, -5,  -5,  -5,  -5,  -5, -5, -5,
                                -10,  -5,  -5,  -5,  -5,  -5, -5,-10,
                                -10, -5, -5, -5, -5, -5, -5,-10,
                                -20,-10,-10, -0, -0,-10,-10,-20]
                         relboard = if c==White then board else reverse board
{--
//queen
-20,-10,-10, -5, -5,-10,-10,-20,
-10,  0,  0,  0,  0,  0,  0,-10,
-10,  0,  5,  5,  5,  5,  0,-10,
 -5,  0,  5,  5,  5,  5,  0, -5,
  0,  0,  5,  5,  5,  5,  0, -5,
-10,  5,  5,  5,  5,  5,  0,-10,
-10,  0,  5,  0,  0,  0,  0,-10,
-20,-10,-10, -5, -5,-10,-10,-20
--}
evalKing :: Colour -> Pos -> Int
evalKing c (m,n) = relboard !! i
                     where
                         i = (m * 8) + n
                         board = [-30,-40,-40,-50,-50,-40,-40,-30,
                                -30,-40,-40,-50,-50,-40,-40,-30,
                                -30,-40,-40,-50,-50,-40,-40,-30,
                                -30,-40,-40,-50,-50,-40,-40,-30,
                                -20,-30,-30,-40,-40,-30,-30,-20,
                                -10,-20,-20,-20,-20,-20,-20,-10,
                                 0, 0,  0,  -5,  -5,  -5, 0, 0,
                                 20, 30, 20,  0,  0, 0, 30, 20]
                         relboard = if c==White then board else reverse board




evalPiece :: Piece -> AllPieces -> Int
--evalPiece (King,_,_,_) ps = 0.0
--evalPiece (Pawn,_,_,_) ps = 0.0
--evalPiece (Rook,_,_,_) ps = 0.0
--evalPiece (Queen,_,_,_) ps = 0.0
--evalPiece a ps = fromIntegral (length (legalMoves a [a]) + length (legalMoves a ps)) -- * pieceMobMult a
--evalPiece a ps = length (legalMoves a [a]) + length (legalMoves a ps) `div` 2-- * pieceMobMult a
--evalPiece a ps = 3 * length (legalMoves a [a])
--evalPiece a ps = length (legalMoves a ps)
evalPiece (Pawn,c,(m,n),mc) ps = evalPawn c (m,n)
evalPiece (Knight,c,(m,n),mc) ps = evalKnight c (m,n) -- + length (legalMoves (Knight,c,(m,n),mc) ps)
evalPiece (Bishop,c,(m,n),mc) ps = evalBishop c (m,n) -- + length (legalMoves (Bishop,c,(m,n),mc) ps)
evalPiece (Rook,c,(m,n),mc) ps = evalRook c (m,n)
evalPiece (Queen,c,(m,n),mc) ps = evalQueen c (m,n)
evalPiece (King,c,(m,n),mc) ps = evalKing c (m,n)

--pieceMobMult :: Piece -> Float
--pieceMobMult (Knight,_,_,_) = 1
--pieceMobMult (Queen,_,_,_) = 0.0
--pieceMobMult (Rook,_,_,_) = 0.25
--pieceMobMult (Bishop,_,_,_) = 1.0

totalMaterial :: Colour -> AllPieces -> Int
totalMaterial c ps = 1 * sum [pieceVal (y,White,(0,0),0) * countPieceType c y ps  | y <- pieceTypes]

countPieceType :: Colour -> PieceType -> AllPieces -> Int
countPieceType c t ps = length [ x | x <- ps, getColour x == c, getPieceType x == t, getPos x /= (-1,-1) ]

pieceTypes :: [PieceType]
pieceTypes = [Pawn, Knight, Bishop, Rook, Queen, King]

totalMobility :: Colour -> AllPieces -> Int
totalMobility c ps = sum [ evalPiece x ps | x <- ps, getColour x == c, getPos x /= (-1,-1) ]

totalOpeningVal :: AllPieces -> Int
totalOpeningVal ps = totalOpeningValColour White ps - totalOpeningValColour Black ps

totalOpeningValColour :: Colour -> AllPieces -> Int
--totalOpeningValColour c ps = totalMaterial c ps + totalMobility c ps + centralPawns c ps + blockedPawns c ps + kingSafety c ps + queenSafety c ps
--totalOpeningValColour c ps = totalMaterial c ps + totalMobility c ps + kingSafety c ps + queenSafety c ps + centralPawns c ps + blockedPawns c ps
--totalOpeningValColour c ps = movePieceBonus c ps + totalMaterial c ps + kingSafety c ps + queenSafety c ps + centralPawns c ps + blockedPawns c ps
--totalOpeningValColour c ps = totalMaterial c ps + totalMobility c ps + queenSafety c ps -- + blockedPawns c ps
totalOpeningValColour c ps = totalMobility c ps + totalMaterial c ps -- + queenSafety c ps + blockedPawns c ps

centralPawns :: Colour -> AllPieces -> Int
centralPawns c ps = 1 * fromIntegral (length [x | x <- ps, getColour x == c, getPieceType x == Pawn, any (==getPos x) squares])
                    where
                        squares = [(3,3),(3,4),(4,3),(4,4)]

blockedPawns :: Colour -> AllPieces -> Int
blockedPawns c ps = length [x | x <- ps, getColour x == c, (getColumn (getPos x) == 3 || getColumn (getPos x) == 4), length (legalMoves x ps) == 0] * (-20)

queenSafety :: Colour -> AllPieces -> Int
queenSafety c ps | q == (-1,-1) = 0
                 | otherwise = movedLessThan c queen ps
                   where
                       queen = head (findPiece q ps)
                       q = findQueen c ps

movedLessThan :: Colour -> Piece -> AllPieces -> Int
movedLessThan c p ps | mc == 0 = 0
                     | otherwise = if null [x | x <- ps, getColour x == c, getMovecount x == 0, (getPieceType x == Knight || getPieceType x == Bishop)] then 0 else (-50)
                       where
                           mc = getMovecount p

castleMotive :: Colour -> AllPieces -> Int
castleMotive c ps | any (==getColumn (findKing c ps)) [3..5] = (-90)
                  | otherwise = 0

staticKingMotive :: Colour -> AllPieces -> Int
staticKingMotive c ps | getRow (findKing c ps) /= r = (-60)
                      | otherwise = 0
                        where r = if c == White then 7 else 0

kingSafety :: Colour -> AllPieces -> Int
kingSafety c ps = castleMotive c ps + staticKingMotive c ps

pieceVal :: Piece -> Int
pieceVal (Pawn,_,_,_)   = 100 --1.0
pieceVal (Knight,_,_,_) = 300 --3.0
pieceVal (Bishop,_,_,_) = 350 -- 3.5
pieceVal (Rook,_,_,_)   = 500 --5.0
pieceVal (Queen,_,_,_)  = 900 --9.0
pieceVal (King,_,_,_)   = 0


-- if a piece is going to be captured then it doesnt really have any material
pieceMaterial :: Piece -> AllPieces -> Float
pieceMaterial a ps | (length t2 > length pr) && not (compareBackupVals t2 pr) = 0
--pieceMaterial a ps | not (compareBackupVals t2 pr) = 0
                   | getLowestVal t < fromIntegral v = 0
                   | otherwise = fromIntegral v
                       where
                             t = threatenedBy a ps
                             t2 = trulyThreatenedBy 4 a ps
                             v = pieceVal a
                             pr= trulyProtectedBy 4 a ps


--trulyThreatenedBy
trulyThreatenedBy :: Int -> Piece -> AllPieces -> [Piece]
trulyThreatenedBy 0 p ps = []
trulyThreatenedBy i p ps = if null xs then [] else xs ++ trulyThreatenedBy (i-1) p newps
                         where
                             xs = sortPieces (threatenedBy p ps)
                             newps = [n | n <- ps, all (/=n) xs]


--trulyProtectedBy
trulyProtectedBy :: Int -> Piece -> AllPieces -> [Piece]
trulyProtectedBy i (p,c,pos,mc) ps = trulyThreatenedBy i newp (newp:removePiece (p,c,pos,mc) ps)
                                     where
                                         newp = (p,invertColour c, pos,mc)

-- returns true if protection is sufficient (xs is threats and ys is protection)
compareBackupVals :: [Piece] -> [Piece] -> Bool
compareBackupVals [] [] = True
compareBackupVals xs [] = False
compareBackupVals [] ys = True
compareBackupVals (x:xs) (y:ys) = if (pieceVal y > pieceVal x) && (length (x:xs) > length (y:ys)) then False else compareBackupVals xs ys


-- sort a list of pieces by value from lowest to highest
sortPieces :: [Piece] -> [Piece]
sortPieces []     = []
sortPieces (p:xs) = (sortPieces lesser) ++ [p] ++ (sortPieces greater)
    where
        lesser  = filter (\y -> pieceVal y < pieceVal p) xs--filter (< p) xs
        greater = filter (\y -> pieceVal y >= pieceVal p) xs--filter (>= p) xs

-- returns the value of the lowest value piece in a list of pieces
getLowestVal :: [Piece] -> Float
getLowestVal ps | null a = 10.0 -- return a value greater than any piece
                | otherwise = fromIntegral (head a)
                  where
                      a = [pieceVal x | x <- ps, all (\y -> (pieceVal y) >= pieceVal x) ps ]



-- add bonus for moving multiple pieces.
movePieceBonus :: Colour -> AllPieces -> Int
movePieceBonus c ps = length [x | x <- ps, getPos x /= (-1,-1), getColour x == c, getMovecount x == 0, getPieceType x == Knight || getPieceType x == Bishop] * (-80)
