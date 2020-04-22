import Util
import TypeDefs
import Init
import Debug

-- some crude evaluations

evalPiece :: Piece -> AllPieces -> Float
evalPiece a ps = fromIntegral (length (legalMoves a ps)) * (pieceVal a)

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_) = 1.0
pieceVal (Knight,_,_,_) = 3.0
pieceVal (Bishop,_,_,_) = 3.5
pieceVal (Rook,_,_,_) = 5
pieceVal (Queen,_,_,_) = 9

totalVal :: Colour -> AllPieces -> Float
totalVal a ps = sum [ evalPiece x ps | x <- ps, getColour x == a ]

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


-- returns whether a pawn is a passed pawn
--isPassedPawn :: Piece -> AllPieces -> Bool
--isPassedPawn a ps =
