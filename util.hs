import TypeDefs
import Init

-- GETTERS

-- returns the column of a position
getColumn :: Pos -> Int
getColumn (_,y) = y

-- returns the row of a position
getRow :: Pos -> Int
getRow (x,_) = x

-- returns the colour of a piece
getColour :: Piece -> Colour
getColour (_,x,_) = x

-- returns the position of a piece
getPos :: Piece -> Pos
getPos (_,_,x) = x

-- returns the starting position of a move
--getStart :: Move -> Pos
--getStart (x,_,_) = getPos x

-- returns the target position for a move
getTarget :: Pos -> Move -> Pos
getTarget (a, b) (c, d) = (a+b, c+d)

-- returns the piece that is on a square in a list (empty list if no piece there)
findPiece :: Pos -> AllPieces -> [Piece]
findPiece a b = [x | x <- b, getPos x == a]



-- UTILITIES AND RULES

-- returns true if there is no piece on pos
isEmpty :: Pos -> AllPieces -> Bool
isEmpty a b = (findPiece a b) == []

-- returns true if the two pieces are enemies
isEnemy :: Piece -> Piece -> Bool
isEnemy a b = getColour a /= getColour b

isOnBoard :: Piece -> Move -> Bool
isOnBoard a b = m >= 0 && m <= 7 && n >= 0 && n <= 7
                where
                    m = getRow (getTarget (getPos a) b)
                    n = getCol (getTarget (getPos a) b)

-- returns whether a square is not occupied by a friendly piece
isValidTarget :: Piece -> Move -> AllPieces -> Bool
isValidTarget a b c = (isEmpty (getTarget (getPos a) b) c) && (isEnemy a z) && isOnBoard a b
                      where z = head (findPiece (getTarget (getPos a) b) c)

-- returns whether a square is occupied by an enemy
isTargetEnemy :: Piece -> Move -> AllPieces -> Bool
isTargetEnemy a b c = isEnemy a z
                      where z = head(findPiece (getTarget (getPos a) b) c)

-- get an int closer to 0
closerToZero :: Int -> Int
closerToZero a | a < 0 = a + 1
               | otherwise = a - 1

-- decrease the value of a move by 1 closer to the original position
decreaseMove :: Move -> Move
decreaseMove (a,b) = (closerToZero a, closerToZero b)

--checks to see if a piece can move along a diagonal line without hitting any pieces.
isDiagonalMovePathEmpty :: Pos -> Move -> AllPieces -> Bool
isDiagonalMovePathEmpty a (0,0) c = True
isDiagonalMovePathEmpty a b c = isEmpty (getTarget a b2) c && isDiagonalMovePathEmpty a b2 c
                                where b2 = decreaseMove b

--returns whether a pawn move is a capture
isPawnCapture :: Piece -> Move -> Bool
isPawnCapture (_,Black,_) (a,b) = b == 1 && abs a == 1
isPawnCapture(_,White,_) (a,b)  = b == -1 && abs a == 0

--returns whether a pawn move is a regular pawn move
isBasicPawnMove :: Piece -> Move -> Bool
isBasicPawnMove (_,Black,_) (a,b) = b == 1 && a == 0
isBasicPawnMove (_,White,_) (a,b) = b == -1 && a == 0

-- returns whether a move is in a straight line or not
isStraightMove :: Move -> Bool
isStraightMove (a,b) = (a == b) || (a == 0 || b == 0)

-- return whether a move is a diagonal
isDiagonal :: Move -> Bool
isDiagonal (a,b) = abs a == abs b

-- returns whether a move is L-shaped.
isLShaped :: Move -> Bool
isLShaped (a,b) = (abs a == 2 && abs b == 1) || (abs a == 1 && abs b == 2)

-- returns whether a pawn move is valid
isPawnValidMove :: Piece -> Move -> AllPieces -> Bool
isPawnValidMove a b c = isValidTarget a b c && ((isTargetEnemy a b c && isPawnCapture a b) || (isEmpty (getTarget (getPos a) b) c && isBasicPawnMove a b))

-- returns whether a knight move is valid
isKnightValidMove :: Piece -> Move -> AllPieces -> Bool
isKnightValidMove a b c = isValidTarget a b c && isLShaped b

-- returns whether a bishop move is valid
isBishopValidMove :: Piece -> Move -> AllPieces -> Bool
isBishopValidMove a b c = isValidTarget a b c && isDiagonal b && isDiagonalMovePathEmpty (getPos a) b c


validKingMove :: Piece -> Move -> AllPieces -> Bool
validKingMove a (m,n) b = (abs m >=1 && abs n >=1) && isValidTarget a (m,n) b

isValidMove :: Piece -> Move -> AllPieces -> Bool
isValidMove (Pawn, col, pos) x y   = isPawnValidMove (Pawn, col, pos) x y
isValidMove (Knight, col, pos) x y = isKnightValidMove (Knight, col, pos) x y
isValidMove (Bishop, col, pos) x y = isBishopValidMove (Bishop, col, pos) x y
