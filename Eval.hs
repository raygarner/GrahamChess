module Eval where

import           Debug
import           Init
import           TypeDefs
import           Util

-- some crude evaluations

evalPiece :: Piece -> AllPieces -> Float
evalPiece a ps = fromIntegral (length (legalMoves a ps))

evalPieceBonus :: Piece -> AllPieces -> Float
evalPieceBonus a ps = (threatenKing a ps) + (protectedEvaluation a ps) + (threatenEvaluation a ps) + (evaluationCentralSquares a ps)

totalMaterial :: Colour -> AllPieces -> Float
totalMaterial c ps = ( (8 * (sum [ pieceMaterial x ps | x <- ps, getPos x /= (-1,-1), getColour x == c ])) - (8 * (sum [ pieceMaterial y ps | y <- ps, getPos y /= (-1,-1), getColour y /= c ])) )

totalMobility :: Colour -> AllPieces -> Float
totalMobility c ps = (sum [ evalPiece x ps | x <- ps, getColour x == c ]) - (sum [ evalPiece y ps | y <- ps, getColour y /= c])

totalBonus :: Colour -> AllPieces -> Float
totalBonus c ps = (sum [evalPieceBonus x ps | x <- ps, getColour x == c]) - (sum [evalPieceBonus y ps | y <- ps, getColour y /= c])

allPawns :: Colour -> AllPieces -> Float
allPawns c ps = (sum [passPawnScore x ps | x <- ps, getColour x == c, getPieceType x == Pawn]) - (sum[passPawnScore y ps | y <- ps, getColour y /= c, getPieceType y == Pawn])

totalVal :: Colour -> AllPieces -> Float
totalVal a ps = (totalMobility a ps) + (totalMaterial a ps) + (totalBonus a ps) + (allPawns a ps) + (fromIntegral (castleBonus a ps))

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_)   = 1.0
pieceVal (Knight,_,_,_) = 3.0
pieceVal (Bishop,_,_,_) = 3.5
pieceVal (Rook,_,_,_)   = 5.0
pieceVal (Queen,_,_,_)  = 9.0
pieceVal (King,_,_,_)   = 0.0

-- TODO: function to return how close a colour is to castling.

castlingPiece :: Piece -> Bool
castlingPiece a = getPieceType a == Queen || getPieceType a == Bishop || getPieceType a == Knight

closeToCastling :: Colour -> Bool -> AllPieces -> [Piece]
closeToCastling c True ps = [x | x <- ps, getColour x == c, castlingPiece x, getMovecount x == 0, getColumn (getPos x) == 5 || getColumn (getPos x) == 6]
closeToCastling c False ps = [x | x <- ps, getColour x == c, castlingPiece x, getMovecount x == 0, getColumn (getPos x) == 1 || getColumn (getPos x) == 2 || getColumn (getPos x) == 3]

castleBonus :: Colour -> AllPieces -> Int
castleBonus c ps | possibleToCastle c True ps && possibleToCastle c False ps = (min (length (closeToCastling c True ps)) (length (closeToCastling c False ps))) * (-2)
                 | possibleToCastle c True ps = (length (closeToCastling c True ps)) * (-2)
                 | possibleToCastle c False ps = (length (closeToCastling c False ps)) * (-2)
                 | otherwise = 0

-- returns true if the king is surrounded by friendly pieces.
isKingSurrounded :: Piece -> AllPieces -> Bool
isKingSurrounded a b = length x == length y
                 where y = getSurroundingPos (getPos a)
                       x = surroundingPieces (getColour a) y b


-- returns a float value for whether a piece is aimed at the enemy king.
threatenKing :: Piece -> AllPieces -> Float
threatenKing a b | isPieceAimedAtEnemyKing a b = 3.0
                 | otherwise = 0.0

-- returns a bool value for whether a piece is aimed at an enemy king.
isPieceAimedAtEnemyKing :: Piece -> AllPieces -> Bool
isPieceAimedAtEnemyKing a b = isValidMove a (moveMade (getPos a) k) (a : [])
                              where k = findKing (invertColour (getColour a)) b

-- protection evaluation
protectedEvaluation :: Piece -> AllPieces -> Float
protectedEvaluation a b = analyzePieces a (protecting a b)


-- analyze the list of all pieces to return a float value for that list - currently used for threaten / protect
analyzePieces :: Piece -> [Piece] -> Float
analyzePieces a [] = 0
analyzePieces a xs | pieceVal a < pieceVal y = ((pieceVal y - pieceVal a) * 2) + analyzePieces a (tail xs)
                   | otherwise = 2.0 + analyzePieces a (tail xs)
                     where
                       y = head xs

-- Tpositive evaluation for threaten
threatenEvaluation :: Piece -> AllPieces -> Float
threatenEvaluation a b = analyzePieces a (threatening a b)

-- crude central square evaluation - if a piece controls 1 or more central squares the return value is 1.5
evaluationCentralSquares :: Piece -> AllPieces -> Float
evaluationCentralSquares a b | null (doesPieceControlCentralSquares a b) = 0.0
                             | otherwise = 3.0

centralSquares :: [Pos]
centralSquares = [(row,col) | row <- [3..4], col <- [3..4]]

-- returns all the squares that a piece threatens from a passed list of squares.
doesPieceThreatenSquares :: Piece -> [Pos] -> AllPieces -> [Pos]
doesPieceThreatenSquares a [] b = []
doesPieceThreatenSquares a xs b | isValidMove a y b = head xs : doesPieceThreatenSquares a (tail xs) b
                                | otherwise = doesPieceThreatenSquares a (tail xs) b
                                  where y = moveMade (getPos a) (head xs)

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

-- if a piece is going to be captured then it doesnt really have any material NOT FULL COMPLETE: NEEDS TO TAKE VALUE OF THREATS INTO ACCOUNT
pieceMaterial :: Piece -> AllPieces -> Float
pieceMaterial a ps | (length (threatenedBy a ps) > length (protectedBy a ps)) = 0
                   | getLowestVal (threatenedBy a ps) < pieceVal a = 0
                   | otherwise = pieceVal a

-- returns the value of the lowest value piece in a list of pieces
getLowestVal :: [Piece] -> Float
getLowestVal ps = head [pieceVal x | x <- ps, all (\y -> (pieceVal y) > pieceVal x) ps ]

-- returns whether all pieces have moved at least once
allPiecesMoved :: AllPieces -> Bool
allPiecesMoved ps = length [ x | x <- ps, getMovecount x > 0, getPieceType x /= Pawn ] >= 16

-- returns whether there are no queens on the board
noQueens :: AllPieces -> Bool
noQueens ps = null [ x | x <- ps, getPieceType x == Queen, getPos x /= (-1,-1) ]

-- returns whether there is a low ammount of material on the board
lowMaterial :: AllPieces -> Bool
lowMaterial ps = length [ x | x <- ps, getPos x /= (-1,-1), getPieceType x /= Pawn, getPieceType x /= King ] <= 8

-- return what point the game is in
getGamePoint :: AllPieces -> GamePoint
getGamePoint ps | (noQueens ps || lowMaterial ps) = End
                | allPiecesMoved ps = Middle
                | otherwise = Opening
