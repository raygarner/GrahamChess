import TypeDefs

-- GETTERS

-- returns the column of a position
getcolumn :: Pos -> Int
getcolumn (_,y) = y

-- returns the row of a position
getrow :: Pos -> Int
getrow (x,_) = x

-- returns the colour of a piece
getcolour :: Piece -> Col
getcolour (_,x,_) = x

-- returns the position of a piece
getpos :: Piece -> Pos
getpos (_,_,x) = x

-- returns the starting position of a move
getStart :: Move -> Pos
getStart (x,_,_) = getpos x

-- returns the target position for a move
getTarget :: Move -> Pos
getTarget (_,y,_) = y

-- returns the piece that is on a square in a list (empty list if no piece there)
getPiece :: Pos -> AllPieces -> [Piece]
getPiece a b = [x | x <- b, getpos x == a]



-- UTILITIES

-- returns true if there is no piece on pos
isEmpty :: Pos -> AllPieces -> Bool
isEmpty a b = (getPiece a b) == []

-- returns true if the two pieces are enemies
isEnemy :: Piece -> Piece -> Bool
isEnemy a b = getcolour a /= getcolour b

-- returns whether a square is not occupied by a friendly piece
isValidTarget :: Move -> AllPieces -> Bool
isValidTarget a b = isEmpty (getTarget a) b || isEnemy s t
                    where
                        s = head (getPiece (getStart a) b)
                        t = head (getPiece (getTarget a) b)

