module EvalOpening where

import           Debug
import           Init
import           TypeDefs
import           Util

-- some crude evaluations

evalPiece :: Piece -> AllPieces -> Float
evalPiece a ps = fromIntegral (length (legalMoves a ps)) * ((10.0 - (pieceVal a)) * 2)

evalPieceBonus :: Piece -> AllPieces -> Float
evalPieceBonus a ps = (threatenKing a ps) + (threatenEvaluation a ps) + (evaluationCentralSquares a ps)

totalMaterial :: Colour -> AllPieces -> Float
totalMaterial c ps = ( (sum [ 100 * pieceMaterial x ps | x <- ps, getPos x /= (-1,-1), getColour x == c ]) - (sum [ 100 * pieceMaterial y ps | y <- ps, getPos y /= (-1,-1), getColour y /= c ]) )

totalMobility :: Colour -> AllPieces -> Float
totalMobility c ps = ( 5 * sum [ evalPiece x ps | x <- ps, getColour x == c, getPos x /= (-1,-1) ]) - (5 * sum [ evalPiece y ps | y <- ps, getColour y /= c, getPos y /= (-1,-1) ])

totalBonus :: Colour -> AllPieces -> Float
totalBonus c ps = (sum [evalPieceBonus x ps | x <- ps, getColour x == c]) - (sum [evalPieceBonus y ps | y <- ps, getColour y /= c])

totalColourBonus :: Colour -> AllPieces -> Float
totalColourBonus a ps = (allPawns a ps) + (cornerKingBonus a ps) + fromIntegral(castleBonus a ps) + fromIntegral (pawnCenterControl a ps) + fromIntegral (movePieceBonus a ps)

allPawns :: Colour -> AllPieces -> Float
allPawns c ps = (sum [passPawnScore x ps | x <- ps, getColour x == c, getPieceType x == Pawn]) - (sum[passPawnScore y ps | y <- ps, getColour y /= c, getPieceType y == Pawn])

pawnCenterControl :: Colour -> AllPieces -> Int
pawnCenterControl colour ps = (length [ x | x <- ps, getPieceType x == Pawn, y <- pawnControlledSquares x, any (==y) centralSquares ]) + (length [ z | z <- ps, getPieceType z == Pawn, any (==getPos z) centralSquares ]) * 100

totalOpeningVal :: Colour -> AllPieces -> Float
totalOpeningVal a ps = (totalMobility a ps) + (totalMaterial a ps) + (totalBonus a ps) + (totalColourBonus a ps)

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_)   = 1.0
pieceVal (Knight,_,_,_) = 3.0
pieceVal (Bishop,_,_,_) = 3.5
pieceVal (Rook,_,_,_)   = 5.0
pieceVal (Queen,_,_,_)  = 9.0
pieceVal (King,_,_,_)   = 0.0



-- get a bonus for performing a castle

isKingInCorner :: Colour -> Pos -> Bool
isKingInCorner colour pos | colour == White = any (== pos) [(7,6), (7,2), (7,1)]
                          | otherwise = any (== pos) [(0,6),(0,2),(0,1)]

-- returns true if the king is surrounded by friendly pieces.
isKingSurrounded :: Pos -> Colour -> AllPieces -> Bool
isKingSurrounded p c ps = length x == length y
                        where
                          y = getSurroundingPos p
                          x = surroundingPieces c y ps

cornerKingBonus :: Colour -> AllPieces -> Float
cornerKingBonus c ps | (isKingInCorner c kingPos) && (isKingSurrounded kingPos c ps) = 75.0
                     | isKingInCorner c kingPos = 50.0
                     | otherwise = 0.0
                       where
                         kingPos = (findKing c ps)

-- castling bonus functions

castlingPiece :: Piece -> Bool
castlingPiece a = getPieceType a == Queen || getPieceType a == Bishop || getPieceType a == Knight

closeToCastling :: Colour -> Bool -> AllPieces -> [Piece]
closeToCastling c True ps = [x | x <- ps, getColour x == c, castlingPiece x, getMovecount x == 0, getColumn (getPos x) == 5 || getColumn (getPos x) == 6]
closeToCastling c False ps = [x | x <- ps, getColour x == c, castlingPiece x, getMovecount x == 0, getColumn (getPos x) == 1 || getColumn (getPos x) == 2 || getColumn (getPos x) == 3]

castleBonus :: Colour -> AllPieces -> Int
castleBonus c ps | possibleToCastle c True ps && possibleToCastle c False ps = (min (length (closeToCastling c True ps)) (length (closeToCastling c False ps))) * (-10)
                  | possibleToCastle c True ps = (length (closeToCastling c True ps)) * (-10)
                  | possibleToCastle c False ps = (length (closeToCastling c False ps)) * (-10)
                  | otherwise = 0

-- add bonus for moving multiple pieces.
movePieceBonus :: Colour -> AllPieces -> Int
movePieceBonus c ps = (length [x | x <- ps, getColour x == c, getMovecount x == 0, getPieceType x == Knight || getPieceType x == Bishop]) * (-500)



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
analyzeProtection xs = (9 - pieceVal (head xs)) + analyzeProtection (tail xs)

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

-- crude central square evaluation - if a piece controls 1 or more central squares the return value is 1.5
evaluationCentralSquares :: Piece -> AllPieces -> Float
evaluationCentralSquares p ps | null (doesPieceControlCentralSquares p ps) = 0.0
                              | otherwise = fromIntegral (length (doesPieceControlCentralSquares p ps)) * 100.0

centralSquares :: [Pos]
centralSquares = [(row,col) | row <- [3..4], col <- [3..4]]

-- returns all the squares that a piece threatens from a passed list of squares.
doesPieceThreatenSquares :: Piece -> [Pos] -> AllPieces -> [Pos]
doesPieceThreatenSquares p [] ps = []
doesPieceThreatenSquares p xs ps | isValidMove p y ps = head xs : doesPieceThreatenSquares p (tail xs) ps
                                 | otherwise = doesPieceThreatenSquares p (tail xs) ps
                                    where
                                      y = moveMade (getPos p) (head xs)

-- returns all the central squares that a piece controls
doesPieceControlCentralSquares :: Piece -> AllPieces -> [Pos]
doesPieceControlCentralSquares (Pawn, col, pos, mc) b = [x | x <- centralSquares, elem x (pawnControlledSquares (Pawn,col,pos,mc))]
doesPieceControlCentralSquares a b = doesPieceThreatenSquares a centralSquares b



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
