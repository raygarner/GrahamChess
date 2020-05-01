module EvalEnd where

import           Debug
import           Init
import           TypeDefs
import           Util

evalPieceBonus :: Piece -> AllPieces -> Float
evalPieceBonus a ps = (threatenKing a ps) + (threatenEvaluation a ps)

totalMaterial :: Colour -> AllPieces -> Float
totalMaterial c ps = ( (sum [ 200 * pieceMaterial x ps | x <- ps, getPos x /= (-1,-1), getColour x == c ]) - (sum [ 200 * pieceMaterial y ps | y <- ps, getPos y /= (-1,-1), getColour y /= c ]) )

totalBonus :: Colour -> AllPieces -> Float
totalBonus c ps = (sum [evalPieceBonus x ps | x <- ps, getColour x == c]) - (sum [evalPieceBonus y ps | y <- ps, getColour y /= c])

totalColourBonus :: Colour -> AllPieces -> Float
totalColourBonus c ps = kingProtection c ps + fromIntegral(getPawnPromotion c ps)


allPawns :: Colour -> AllPieces -> Float
allPawns c ps = (sum [passPawnScore x ps | x <- ps, getColour x == c, getPieceType x == Pawn]) - (sum[passPawnScore y ps | y <- ps, getColour y /= c, getPieceType y == Pawn]) * 100

totalEndVal :: Colour -> AllPieces -> Float
totalEndVal a ps =  (totalMaterial a ps) + (totalBonus a ps)  + (allPawns a ps)

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_)   = 5.0
pieceVal (Knight,_,_,_) = 3.0
pieceVal (Bishop,_,_,_) = 3.5
pieceVal (Rook,_,_,_)   = 5.0
pieceVal (Queen,_,_,_)  = 9.0
pieceVal (King,_,_,_)   = 1.0

-- returns true if the king is surrounded by friendly pieces.
isKingSurrounded :: Pos -> Colour -> AllPieces -> Bool
isKingSurrounded p c ps = length x == length y
                  where y = getSurroundingPos p
                        x = surroundingPieces c y ps

-- gives bonus points if the king is surrounded by at least one piece
kingProtection :: Colour -> AllPieces -> Float
kingProtection c ps | length (surroundingPieces c king ps ) >= 1 = 25.0
                    | otherwise = 0.0
                    where
                      king = getSurroundingPos (findKing c ps)


pawnsNearEnd :: Colour -> AllPieces -> [Piece]
pawnsNearEnd White ps = [x | x <- ps, getColour x == White, getRow (getPos x) == 1, getPieceType x == Pawn]
pawnsNearEnd Black ps = [x | x <- ps, getColour x == Black, getRow (getPos x) == 6, getPieceType x == Pawn]

-- TODO: bonus points for pawns closer to end
getPawnPromotion :: Colour -> AllPieces -> Int
getPawnPromotion c ps = length (pawnsNearEnd c ps) * 15

-- returns a float value for whether a piece is aimed at the enemy king.
threatenKing :: Piece -> AllPieces -> Float
threatenKing p ps | isPieceAimedAtEnemyKing p ps = 5.0
                  | otherwise = 0.0

-- returns a bool value for whether a piece is aimed at an enemy king.
isPieceAimedAtEnemyKing :: Piece -> AllPieces -> Bool
isPieceAimedAtEnemyKing p ps = isValidMove p (moveMade (getPos p) k) (p : [])
                               where
                                 k = findKing (invertColour (getColour p)) ps

-- protection evaluation
protectedEvaluation :: Piece -> AllPieces -> Float
protectedEvaluation p ps = analyzeProtection (protecting p ps)

analyzeProtection :: [Piece] -> Float
analyzeProtection [] = 0
analyzeProtection xs = (10 - pieceVal (head xs)) / 4  + analyzeProtection (tail xs)

-- analyze the list of all pieces to return a float value for that list - currently used for threaten / protect
analyzePieces :: Piece -> [Piece] -> Float
analyzePieces a [] = 0
analyzePieces a xs | pieceVal a < pieceVal y = ((pieceVal y - pieceVal a) * 2) + analyzePieces a (tail xs)
                   | otherwise = 2.0 + analyzePieces a (tail xs)
                     where
                       y = head xs

-- Tpositive evaluation for threaten
threatenEvaluation :: Piece -> AllPieces -> Float
threatenEvaluation p ps = analyzePieces p (threatening p ps)


passPawnScore :: Piece -> AllPieces -> Float
passPawnScore a ps | isPassedPawn a ps = 3.0
                   | otherwise = 0

-- returns whether a position doesnt contain an enemy pawn
isNotEnemyPawn :: Colour -> Pos -> AllPieces -> Bool
isNotEnemyPawn c p ps = isEmpty p ps || (getPieceType (head (findPiece p ps))) /= Pawn

-- returns whether the 3 squares in front of one are not enemy pawns
pawnClearAhead :: Colour -> Pos -> AllPieces -> Bool
pawnClearAhead c (m,n) ps = all (==True) [ isNotEnemyPawn c (m+d, n+x) ps | x <- [-1..1] ]
                            where
                                d = if c == White then -1 else 1

-- returns whether a pawn is a passed pawn
isPassedPawn :: Piece -> AllPieces -> Bool
isPassedPawn a ps = all (==True) [pawnClearAhead (getColour a) (y,n) ps | y <- [m,m+d..e]]
                    where
                        (m,n) = getPos a
                        d = if getColour a == White then -1 else 1
                        e = if getColour a == White then 1 else 6

-- if a piece is going to be captured then it doesnt really have any material
pieceMaterial :: Piece -> AllPieces -> Float
pieceMaterial a ps | (length (threatenedBy a ps) > length (protectedBy a ps)) = 0 - pieceVal a
                   | getLowestVal (threatenedBy a ps) < pieceVal a = 0 - pieceVal a
                   | otherwise = pieceVal a

-- returns the value of the lowest value piece in a list of pieces
getLowestVal :: [Piece] -> Float
getLowestVal ps | null a = 10.0 -- return a value greater than any piece
                | otherwise = head a
                  where
                      a = [pieceVal x | x <- ps, all (\y -> (pieceVal y) >= pieceVal x) ps ]
