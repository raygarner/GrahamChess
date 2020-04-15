import TypeDefs

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
getStart :: Move -> Pos
getStart (x,_,_) = getPos x

-- returns the target position for a move
getTarget :: Move -> Pos
getTarget (_,y,_) = y

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
isValidTarget :: Move -> AllPieces -> Bool
isValidTarget a b = isEmpty (getTarget a) b || isEnemy s t
                    where
                        s = head (findPiece (getStart a) b)
                        t = head (findPiece (getTarget a) b)

-- returns whether a move is in a straight line or not
isStraightMove :: Move -> Bool
isStraightMove a = (colDiff == rowDiff) || (colDiff == 0 || rowDiff == 0)
                   where
                       colDiff = abs (getColumn (getTarget a)) - (getColumn (getStart a))
                       rowDiff = abs (getRow (getTarget a)) - (getRow (getStart a))

