module EvalEnd where

import           Debug
import           Init
import           TypeDefs
import           Util

totalMaterial :: Colour -> AllPieces -> Float
totalMaterial c ps = ((sum [pieceMaterial x ps | x <- ps, getPos x /= (-1,-1), getColour x == c ]) - (sum [pieceVal y | y <- ps, getPos y /= (-1,-1), getColour y /= c ])) * 1.25

-- if a piece is going to be captured then it doesnt really have any material
pieceMaterial :: Piece -> AllPieces -> Float
pieceMaterial a ps | (length (threatenedBy a ps) > length (protectedBy a ps)) = (0 - pieceVal a)
                   | getLowestVal (threatenedBy a ps) < pieceVal a = (0 - pieceVal a)
                   | otherwise = pieceVal a

getLowestVal :: [Piece] -> Float
getLowestVal ps | null a = 10.0
                | otherwise = head a
                where
                  a = [pieceVal x | x <- ps, all (\y -> (pieceVal y) >= pieceVal x) ps]


perPieceBonus :: Colour -> AllPieces -> Float
perPieceBonus c ps = (sum[threatenKingBonus x ps | x <- ps, getPos x /= (-1,-1), getColour x == c])

totalColourBonus :: Colour -> AllPieces -> Float
totalColourBonus c ps = getPawnPromotion c ps + isOpposingKingInCheck c ps

allPawns :: Colour -> AllPieces -> Float
allPawns c ps = (sum [passPawnScore x ps | x <- ps, getColour x == c, getPieceType x == Pawn]) - (sum[passPawnScore y ps | y <- ps, getColour y /= c, getPieceType y == Pawn])

totalEndVal :: Colour -> AllPieces -> Float
totalEndVal a ps =  (totalMaterial a ps) + (totalColourBonus a ps) + (allPawns a ps) + (perPieceBonus a ps)

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_)   = 2.5
pieceVal (Knight,_,_,_) = 3.0
pieceVal (Bishop,_,_,_) = 3.5
pieceVal (Rook,_,_,_)   = 5.0
pieceVal (Queen,_,_,_)  = 9.0
pieceVal (King,_,_,_)   = 1.0

isPieceAimedAtEnemyKing :: Piece -> AllPieces -> Bool
isPieceAimedAtEnemyKing p ps = isValidMove p (moveMade (getPos p) k) (p : [])
                               where
                                 k = findKing (invertColour (getColour p)) ps

threatenKingBonus :: Piece -> AllPieces -> Float
threatenKingBonus p ps | isPieceAimedAtEnemyKing p ps = 3.0
                       | otherwise = 0.0

-- TODO : trap enemy king in corner?
isKingOnEdges :: Colour -> AllPieces -> Float
isKingOnEdges c ps = isKingSideCol king + isKingSideRow king
                     where king = findKing (invertColour c) ps

isKingSideCol :: Pos -> Float
isKingSideCol p | getColumn p == 0 || getColumn p == 7 = 2.0
                | otherwise = 0

isKingSideRow :: Pos -> Float
isKingSideRow p | getRow p == 0 || getRow p == 7 = 2.0
                | otherwise = 0

pawnsNearEnd :: Colour -> AllPieces -> [Piece]
pawnsNearEnd White ps = [x | x <- ps, getColour x == White, getRow (getPos x) == 1, getPieceType x == Pawn]
pawnsNearEnd Black ps = [x | x <- ps, getColour x == Black, getRow (getPos x) == 6, getPieceType x == Pawn]

-- bonus points for pawns closer to end
getPawnPromotion :: Colour -> AllPieces -> Float
getPawnPromotion c ps = fromIntegral (length (pawnsNearEnd c ps)) * 1.25

passPawnScore :: Piece -> AllPieces -> Float
passPawnScore a ps | isPassedPawn a ps = 2.5
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

isOpposingKingInCheck :: Colour -> AllPieces -> Float
isOpposingKingInCheck c ps | isKingInCheck king ps = 2.5
                           | otherwise = 0.0
                             where king = head (findPiece (findKing (invertColour c) ps) ps)
