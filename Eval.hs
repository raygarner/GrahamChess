import           Debug
import           Init
import           TypeDefs
import           Util

-- some crude evaluations

evalPiece :: Piece -> AllPieces -> Float
evalPiece a ps = fromIntegral (length (legalMoves a ps)) * (pieceVal a)

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_,_)   = 1.0
pieceVal (Knight,_,_,_) = 3.0
pieceVal (Bishop,_,_,_) = 3.5
pieceVal (Rook,_,_,_)   = 5.0
pieceVal (Queen,_,_,_)  = 9.0

-- returns a list containg all the surrounding friendly pieces from a position
surroundingPieces :: Colour -> [Pos] -> AllPieces -> [Piece]
surroundingPieces a [] b = []
surroundingPieces a xs b | not (null p) && a ==  getColour (head p) = head p : surroundingPieces a (tail xs) b
                         | otherwise = surroundingPieces a (tail xs) b
                           where p = findPiece (head xs) b

-- returns true if the king is surrounded by friendly pieces.
isKingSurrounded :: Piece -> AllPieces -> Bool
isKingSurrounded a b = length x == length y
                 where y = getSurroundingPos (getPos a)
                       x = surroundingPieces (getColour a) y b

isIntOnBoard :: Int -> Bool
isIntOnBoard a | a < 0 || a > 7 = False
               | otherwise = True

getSurroundingPos :: Pos -> [Pos]
getSurroundingPos (m,n) = [ (row,col) | row <- [m-1..m+1], col <- [n-1..n+1], isIntOnBoard row, isIntOnBoard col, (row,col) /= (m,n)]

-- returns a float value for whether a piece is aimed at the enemy king.
threatenKing :: Piece -> AllPieces -> Float
threatenKing a b | isPieceAimedAtEnemyKing a b = 1.5
                 | otherwise = 1.0

-- returns a bool value for whether a piece is aimed at an enemy king.
isPieceAimedAtEnemyKing :: Piece -> AllPieces -> Bool
isPieceAimedAtEnemyKing a b = isValidMove a (moveMade (getPos a) k) (a : [])
                              where k = findKing (invertColour (getColour a)) b

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
