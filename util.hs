import TypeDefs

-- GETTERS
getcolumn :: Pos -> Int
getcolumn (x,_) = x

getrow :: Pos -> Int
getrow (_,y) = y

getcolour :: Piece -> Col
getcolour (_,x,_) = x

getpos :: Piece -> Pos
getpos (_,_,x) = x


-- UTILITIES
--returns a list of pieces on a square
piecesOn :: Pos -> AllPieces -> [Piece]
piecesOn a b = [x | x <- b, getpos x == a]

--returns true if there is no piece on pos
isEmpty :: Pos -> AllPieces -> Bool
isEmpty a b = piecesOn a b == []

-- RULES

-- goal: returns a list of the valid moves a knight can make
--validN :: Square -> Board -> [Move]
--validN a b = [(a, x, True) | x <- b, (getcolumn (getpos x) == getcolumn (getpos a) + 1) && (getrow (getpos x) == getrow (getpos a) + 2)]


