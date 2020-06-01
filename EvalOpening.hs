module EvalOpening where

import           TypeDefs
import           Util
import           Debug
import           Debug.Trace
import           Control.Parallel

-- some crude evaluations

evalPiece :: Piece -> AllPieces -> Float
evalPiece (Queen,_,_,_) ps = 0.0
evalPiece (King,_,_,_) ps = 0.0
evalPiece (Pawn,_,_,_) ps = 0.0
evalPiece a ps = fromIntegral (length (legalMoves a ps)) * pieceMobMult a

pieceMobMult :: Piece -> Float
pieceMobMult (Pawn,_,_,_) = 0.0
pieceMobMult (Knight,_,_,_) = 1.0
pieceMobMult (Queen,_,_,_) = 0.0
pieceMobMult (Rook,_,_,_) = 0.5
pieceMobMult (King,_,_,_) = 0.0
pieceMobMult (Bishop,_,_,_) = 1.0

totalMaterial :: AllPieces -> Float
totalMaterial ps = 100 * sum [pieceVal (y,White,(0,0),0) * (countPieceType White y ps - countPieceType Black y ps) | y <- pieceTypes]

countPieceType :: Colour -> PieceType -> AllPieces -> Float
countPieceType c t ps = fromIntegral (length [ x | x <- ps, getColour x == c, getPieceType x == t, getPos x /= (-1,-1) ])

pieceTypes :: [PieceType]
pieceTypes = [Pawn, Knight, Bishop, Rook, Queen, King]

totalMobility :: Colour -> AllPieces -> Float
totalMobility c ps = if moves==0 then mate else moves
                     where
                         moves = sum [ evalPiece x ps | x <- ps, getColour x == c, getPos x /= (-1,-1) ]
                         mate = -1000000

totalOpeningVal :: AllPieces -> Float
totalOpeningVal ps = (totalMobility White ps - totalMobility Black ps) + totalMaterial ps + (kingSafety White ps - kingSafety Black ps)


castleMotive :: Colour -> AllPieces -> Float
castleMotive c ps | any (==getColumn (findKing c ps)) [3..5] = (-40)
                  | otherwise = 0

staticKingMotive :: Colour -> AllPieces -> Float
staticKingMotive c ps | getRow (findKing c ps) /= r = (-40)
                      | otherwise = 0
                        where r = if c == White then 7 else 0

kingSafety :: Colour -> AllPieces -> Float
kingSafety c ps = castleMotive c ps + staticKingMotive c ps

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_)   = 1.0
pieceVal (Knight,_,_,_) = 3.0
pieceVal (Bishop,_,_,_) = 3.5
pieceVal (Rook,_,_,_)   = 5.0
pieceVal (Queen,_,_,_)  = 9.0
pieceVal (King,_,_,_)   = 0.0


-- if a piece is going to be captured then it doesnt really have any material
pieceMaterial :: Piece -> AllPieces -> Float
--pieceMaterial a ps   | (length t2 > length pr) && not (compareBackupVals t2 pr) = 0
pieceMaterial a ps   | not (compareBackupVals t2 pr) = 0
                     | getLowestVal t < v = 0
                     | otherwise = v
                       where t = threatenedBy a ps
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
                | otherwise = head a
                  where
                      a = [pieceVal x | x <- ps, all (\y -> (pieceVal y) >= pieceVal x) ps ]
