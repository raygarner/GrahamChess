module EvalEnd where

import           Debug
import           Init
import           TypeDefs
import           Util

totalMaterial :: AllPieces -> Float
totalMaterial ps = 50 * sum [pieceVal (y,White,(0,0),0) * (countPieceType White y ps - countPieceType Black y ps) | y <- pieceTypes]

countPieceType :: Colour -> PieceType -> AllPieces -> Float
countPieceType c t ps = fromIntegral (length [ x | x <- ps, getColour x == c, getPieceType x == t, getPos x /= (-1,-1) ])

pieceTypes :: [PieceType]
pieceTypes = [Pawn, Knight, Bishop, Rook, Queen, King]

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
totalColourBonus c ps = isKingOnEdges c ps + getPawnPromotion c ps

allPawns :: Colour -> AllPieces -> Float
allPawns c ps = (sum [passPawnScore x ps | x <- ps, getColour x == c, getPieceType x == Pawn])

checkMateScore :: AllPieces -> Float
checkMateScore ps | isCheckmate White ps = 1000000
                  | isCheckmate Black ps = -1000000
                  | otherwise = 0

totalEndVal :: AllPieces -> Float
totalEndVal ps =  totalMaterial ps + (totalColourBonus White ps - totalColourBonus Black ps) + (allPawns White ps - allPawns Black ps) + checkMateScore ps

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_)   = 2.75
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
threatenKingBonus p ps | isPieceAimedAtEnemyKing p ps = 0.05
                       | otherwise = 0.0

isKingOnEdges :: Colour -> AllPieces -> Float
isKingOnEdges c ps = isKingSideCol king + isKingSideRow king
                     where king = findKing (invertColour c) ps

isKingSideCol :: Pos -> Float
isKingSideCol p | getColumn p == 0 || getColumn p == 7 = 0.9
                | otherwise = 0

isKingSideRow :: Pos -> Float
isKingSideRow p | getRow p == 0 || getRow p == 7 = 0.9
                | otherwise = 0

pawnsNearEnd :: Colour -> AllPieces -> [Piece]
pawnsNearEnd White ps = [x | x <- ps, getColour x == White, (getRow (getPos x) == 1 || getRow (getPos x) == 2), getPieceType x == Pawn]
pawnsNearEnd Black ps = [x | x <- ps, getColour x == Black, (getRow (getPos x) == 6 || getRow (getPos x) == 5), getPieceType x == Pawn]


-- bonus points for pawns closer to end
getPawnPromotion :: Colour -> AllPieces -> Float
getPawnPromotion c ps = fromIntegral (div (length (pawnsNearEnd c ps)) 2)

passPawnScore :: Piece -> AllPieces -> Float
passPawnScore a ps | isPassedPawn a ps = 2.5
                   | otherwise = 0

-- returns whether a position doesnt contain an enemy pawn
isNotEnemyPawn :: Colour -> Pos -> AllPieces -> Bool
isNotEnemyPawn c p ps = isEmpty p ps || (getPieceType (head (findPiece p ps))) /= Pawn

-- returns whether the 3 squares in front of one are not enemy pawns
pawnClearAhead :: Colour -> Pos -> AllPieces -> Bool
pawnClearAhead c (m,n) ps = all (==True) [ isNotEnemyPawn c (m+d, n+x) ps | x <- [-1..1]]
                            where
                                d = if c == White then -1 else 1

-- returns whether a pawn is a passed pawn
isPassedPawn :: Piece -> AllPieces -> Bool
isPassedPawn a ps = all (==True) [pawnClearAhead (getColour a) (y,n) ps| y <- [m,m+d..e]]
                    where
                        (m,n) = getPos a
                        d = if getColour a == White then -1 else 1
                        e = if getColour a == White then 1 else 6

isOpposingKingInCheck :: Colour -> AllPieces -> Float
isOpposingKingInCheck c ps | isKingInCheck king ps = 3.0
                           | otherwise = 0.0
                             where king = head (findPiece (findKing (invertColour c) ps) ps)

isCheckmate :: Colour -> AllPieces -> Bool
isCheckmate c ps = null (allLegalMoves c ps) && isKingInCheck king ps
                   where
                     king = head (findPiece (findKing c ps) ps)
