import           TypeDefs

-- GETTERS

-- returns the column of a position
getColumn :: Pos -> Int
getColumn (_,y) = y

-- returns the row of a position
getRow :: Pos -> Int
getRow (x,_) = x

-- returns the colour of a piece
getColour :: Piece -> Col
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

-- returns whether a square is not occupied by a friendly piece
isValidTarget :: Piece -> Move -> AllPieces -> Bool
isValidTarget a b c = (isEmpty (getTarget (getPos a) b) c) && (isEnemy a z)
                      where z = head (findPiece (getTarget (getPos a) b) c)

-- returns whether a square is occupied by an enemy
isTargetEnemy :: Piece -> Move -> AllPieces -> Bool
isTargetEnemy a b c = isEnemy a z
                      where z = head(findPiece (getTarget (getPos a) b) c)

--returns whether a pawn move is a capture
isPawnCapture :: Piece -> Move -> Bool
isPawnCapture (_,False,_) (a,b) = b == 1 && abs a == 1
isPawnCapture(_,True,_) (a,b)   = b == -1 && abs a == 0

--returns whether a pawn move is a regular pawn move
isBasicPawnMove :: Piece -> Move -> Bool
isBasicPawnMove (_,False,_) (a,b) = b == 1 && a == 0
isBasicPawnMove (_,True,_) (a,b)  = b == -1 && a == 0


-- returns whether a move is in a straight line or not
isStraightMove :: Move -> Bool
isStraightMove (a,b) = (a == b) || (a == 0 || b == 0)

-- returns whether a move is L-shaped.
isLShaped :: Move -> Bool
isLShaped (a,b) = (abs a == 2 && abs b == 1) || (abs a == 1 && abs b == 2)

isValidMove :: Piece -> Move -> AllPieces -> Bool
isValidMove (Pawn, col, pos) x y = isValidTarget (Pawn, col, pos) x y && ((isTargetEnemy (Pawn,col,pos) x y && isPawnCapture (Pawn,col,pos) x) || (isEmpty (getTarget pos x) y && isBasicPawnMove (Pawn,col,pos) x))
isValidMove (Knight, col, pos) x y = isValidTarget (Knight, col, pos) x y && isLShaped x
