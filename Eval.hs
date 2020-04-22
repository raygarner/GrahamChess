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
