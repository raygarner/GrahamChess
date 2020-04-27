module Util where

import           Data.List
import           Debug
import           Debug
import           Init
import           System.Directory
import           TypeDefs

-- GETTERS

-- returns the column of a position
getColumn :: Pos -> Int
getColumn (_,y) = y

-- returns the row of a position
getRow :: Pos -> Int
getRow (x,_) = x

-- returns the colour of a piece
getColour :: Piece -> Colour
getColour (_,x,_,_) = x

-- returns the position of a piece
getPos :: Piece -> Pos
getPos (_,_,x,_) = x

-- returns the type of a piece
getPieceType :: Piece -> PieceType
getPieceType (x,_,_,_) = x

--returns a list with a given piece removed
removePiece :: Piece -> [Piece] -> [Piece]
removePiece pieceToRemove xs = [ x | x <- xs, x /= pieceToRemove]

-- returns what move was made based on two positions
moveMade :: Pos -> Pos -> Move
moveMade (a,b) (c,d) = (c - a, d - b)

-- returns a piece with an updated position
updatePosition :: Piece -> Move -> Piece
updatePosition (a,b,c,mc) d = (a, b, (getTarget c d), mc+1)

-- returns the starting position of a move
--getStart :: Move -> Pos
--getStart (x,_,_) = getPos x

-- returns the target position for a move WORKING
getTarget :: Pos -> Move -> Pos
getTarget (a, b) (c, d) = (a+c, b+d)

-- returns the piece that is on a square in a list (empty list if no piece there) WORKING
findPiece :: Pos -> AllPieces -> [Piece]
findPiece a b = [x | x <- b, getPos x == a]

-- returns the inversion of a colour
invertColour :: Colour -> Colour
invertColour White = Black
invertColour Black = White

-- invert piece colour
invertPieceColour :: Piece -> Piece
invertPieceColour (a,col,pos,mc) = (a, invertColour col, pos, mc)


-- gets the ammount of moves a piece has made
getMovecount :: Piece -> Movecount
getMovecount (_,_,_,mc) = mc

-- UTILITIES AND RULES

-- returns true if there is no piece on pos WORKING
isEmpty :: Pos -> AllPieces -> Bool
isEmpty a b = (findPiece a b) == []

-- returns true if the two pieces are enemies WORKING
isEnemy :: Piece -> Piece -> Bool
isEnemy a b = getColour a /= getColour b

-- returns true if the two piece are friendly
isFriendly :: Piece -> Piece -> Bool
isFriendly a b = getColour a == getColour b

-- returns whether a move will mean the piece ends up off of the board or not -- WORKING
isOnBoard :: Piece -> Move -> Bool
isOnBoard a b = m >= 0 && m <= 7 && n >= 0 && n <= 7
                where
                    m = getRow (getTarget (getPos a) b)
                    n = getColumn (getTarget (getPos a) b)

-- returns whether a square is not occupied by a friendly piece WORKING
isValidTarget :: Piece -> Move -> AllPieces -> Bool
isValidTarget a b c = ((isEmpty (getTarget (getPos a) b) c) || (isEnemy a z)) && isOnBoard a b
                      where z = head (findPiece (getTarget (getPos a) b) c)

-- returns whether a square is occupied by an enemy -- WORKING
isTargetEnemy :: Piece -> Move -> AllPieces -> Bool
isTargetEnemy a b c = not (isEmpty (getTarget (getPos a) b) c) && isEnemy a z -- checks whether square is empty to prevent empty list being passed to head and causing an error - ray
                      where z = head(findPiece (getTarget (getPos a) b) c)

-- get an int closer to 0
closerToZero :: Int -> Int
closerToZero a | a < 0 = a + 1
               | a > 0 = a - 1 -- added this line so -1 isnt returned if a is 0 - from ray
               | otherwise = a

-- decrease the value of a move by 1 closer to the original position
decreaseDiagonalMove :: Move -> Move
decreaseDiagonalMove (a,b) = (closerToZero a, closerToZero b)


--checks to see if a piece can move along a diagonal line without hitting any pieces.
isDiagonalMovePathEmpty :: Pos -> Move -> AllPieces -> Bool
isDiagonalMovePathEmpty a (0,0) c = True
isDiagonalMovePathEmpty a b c = isEmpty (getTarget a b) c && isDiagonalMovePathEmpty a b2 c
                                where b2 = decreaseDiagonalMove b

-- checks to see if a piece can move along a straight line without hitting any pieces.
isStraightMovePathEmpty :: Pos -> Move -> AllPieces -> Bool
isStraightMovePathEmpty a (0,0) c = True
isStraightMovePathEmpty a (0,b) c = isEmpty (getTarget a (0,b)) c && isStraightMovePathEmpty a (0,b2) c
                                    where b2 = closerToZero b
isStraightMovePathEmpty a (b,0) c = isEmpty (getTarget a (b,0)) c && isStraightMovePathEmpty a (b2,0) c
                                    where b2 = closerToZero b

--returns whether a pawn move is a capture -- WORKING (i think)
isPawnCapture :: Piece -> Move -> Bool
isPawnCapture (_,Black,_,_) (a,b) = a == 1 && abs b == 1
isPawnCapture (_,White,_,_) (a,b) = a == -1 && abs b == 1
-- change made - ray
--isPawnCapture (_,Black,_) (a,b) = b == 1 && abs a == 1
--isPawnCapture(_,White,_) (a,b)  = b == -1 && abs a == 0

--returns whether a pawn move is a regular pawn move -- swapped a and b (row,column) WORKING
isBasicPawnMove :: Piece -> Move -> AllPieces-> Bool
isBasicPawnMove (_,Black,(1,n),_) (a,b)  ps = ((a == 2 && (isStraightMovePathEmpty (1,n) (a,b) ps)) || a == 1) && b == 0
isBasicPawnMove (_,White,(6,n),_) (a,b)  ps = ((a == -2 && (isStraightMovePathEmpty (6,n) (a,b) ps)) || a == -1) && b == 0
isBasicPawnMove (_,Black,_,_) (a,b)      ps = a == 1 && b == 0
isBasicPawnMove (_,White,_,_) (a,b)      ps = a == -1 && b == 0

-- returns whether a pawn move is a promotion
isValidPromotion :: Piece -> Move -> AllPieces -> Bool
isValidPromotion a m ps = getPieceType a == Pawn && isPawnValidMove a m ps && getRow (getTarget (getPos a) m) == e
                          where
                              e = if getColour a == White then 0 else 7

-- changes a piece to a queen
upgradeToQueen :: Piece -> AllPieces -> AllPieces
upgradeToQueen (piece,colour,(m,n),mc) ps = (Queen, colour, (m,n), mc) : (removePiece (piece, colour, (m,n), mc) ps)

-- promotes a pawn
promotePawn :: Piece -> Move -> AllPieces -> AllPieces
promotePawn a m ps = upgradeToQueen piece board
                     where board = executeMove a m ps
                           piece = head (findPiece (getTarget (getPos a) m) board)


-- returns whether a move is in a straight line or not
isStraightMove :: Move -> Bool
isStraightMove (a,b) = (a == 0 || b == 0)

-- return whether a move is a diagonal
isDiagonal :: Move -> Bool
isDiagonal (a,b) = abs a == abs b

-- returns whether a move is L-shaped.
isLShaped :: Move -> Bool
isLShaped (a,b) = (abs a == 2 && abs b == 1) || (abs a == 1 && abs b == 2)

-- returns whether a pawn move is valid
isPawnValidMove :: Piece -> Move -> AllPieces -> Bool
isPawnValidMove a b c = isValidTarget a b c && ( (isEmpty (getTarget (getPos a) b) c && isBasicPawnMove a b c) || (isTargetEnemy a b c && isPawnCapture a b))

-- returns whether a move is a valid en passant move
isValidEnPassant :: Piece -> Move -> AllPieces -> Bool
isValidEnPassant a (m,n) ps = isPawnCapture a (m,n) && getRow (getPos a) == r && not (isEmpty (getTarget (getPos a) (0,n)) ps) && p == (Pawn, invertColour (getColour a), (getRow (getPos a), getColumn (getPos a) + n), 1)
                              where
                                  p = head (findPiece (getTarget (getPos a) (0,n)) ps)
                                  r = if getColour a == White then 3 else 4

-- moves the pawn and removes the adjacent enemy pawn
-- note this can produce an error if used when the move is not a validEnPassant cus of the use of head
captureEnPassant :: Piece -> Move -> AllPieces -> AllPieces
captureEnPassant a (m,n) ps = executeMove a (m,n) (removePiece (head (findPiece ( getTarget (getPos a) (0,n) ) ps )) ps)

-- returns whether a knight move is valid -- WORKING (i think. needs thorough testing)
isKnightValidMove :: Piece -> Move -> AllPieces -> Bool
isKnightValidMove a b c = isValidTarget a b c && isLShaped b

-- returns whether a bishop move is valid
isBishopValidMove :: Piece -> Move -> AllPieces -> Bool
isBishopValidMove a b c = isDiagonal b && isValidTarget a b c && isDiagonalMovePathEmpty (getPos a) (decreaseDiagonalMove b) c
-- are these the same? - from ray
--isBishopValidMove a b c | isDiagonal b = isValidTarget a b c && isDiagonalMovePathEmpty (getPos a) b c
 --                       | otherwise = False

isRookValidMove :: Piece -> Move -> AllPieces -> Bool
isRookValidMove a (0,b) c = isValidTarget a (0,b) c && isStraightMovePathEmpty (getPos a) (0,closerToZero b) c
isRookValidMove a (b,0) c = isValidTarget a (b,0) c && isStraightMovePathEmpty (getPos a) (closerToZero b,0) c
isRookValidMove a b c = False
-- are these the same? - from ray
--isRookValidMove a b c | isStraightMove b = isValidTarget a b c && isStraightMovePathEmpty (getPos a) b c
--                      | otherwise = False


-- returns whether a queen move is valid
isQueenValidMove :: Piece -> Move -> AllPieces -> Bool
isQueenValidMove a b c = isRookValidMove a b c || isBishopValidMove a b c

-- returns the position of the king WORKING
findKing :: Colour -> AllPieces -> Pos
findKing a b = head [p | (t, c, p, m) <- b, t == King, c == a]

-- returns whether a king move is valid WORKING
validKingMove :: Piece -> Move -> AllPieces -> Bool
validKingMove a (m,n) b = (abs m <= 1 && abs n <= 1 ) && isValidTarget a (m,n) b && (x > 1 || y > 1)
                          where
                              x = abs ((getColumn (getTarget (getPos a) (m,n)) - (getColumn (findKing (invertColour (getColour a)) b))))
                              y = abs ((getRow (getTarget (getPos a) (m,n)) - (getRow (findKing (invertColour (getColour a)) b))))


-- returns whether a move is valid
isValidMove :: Piece -> Move -> AllPieces -> Bool
isValidMove (Pawn, col, pos, mc) x y   = isPawnValidMove (Pawn, col, pos, mc) x y
isValidMove (Knight, col, pos, mc) x y = isKnightValidMove (Knight, col, pos, mc) x y
isValidMove (Bishop, col, pos, mc) x y = isBishopValidMove (Bishop, col, pos, mc) x y
isValidMove (Rook, col, pos, mc) x y   = isRookValidMove (Rook, col, pos, mc) x y
isValidMove (Queen, col, pos, mc) x y  = isQueenValidMove (Queen, col, pos, mc) x y
isValidMove (King, col, pos, mc) x y   = validKingMove (King, col, pos, mc) x y

-- returns a list of the pieces which can capture piece a
threatenedBy :: Piece -> AllPieces -> [Piece]
threatenedBy a b = [ x | x <- b, isValidMove x (m - getRow (getPos x), n - getColumn (getPos x)) b ]
                  where
                      m = getRow (getPos a)
                      n = getColumn (getPos a)

-- changes all pieces to reflect the inverted colour.
updateAllPieces :: Piece -> AllPieces -> [Piece]
updateAllPieces a xs = invertPieceColour a : removePiece a xs

-- returns a list of the pieces which
protectedBy :: Piece -> AllPieces -> [Piece]
protectedBy a xs = threatenedBy (invertPieceColour a) (updateAllPieces a xs)

-- returns all pieces that a piece is protecting
protecting :: Piece -> AllPieces -> [Piece]
protecting (King,col,pos,mc) xs = surroundingPieces col (getSurroundingPos pos) xs
protecting a xs = getPiecesFromMoves (getPos a) (legalMoves (invertPieceColour a) (updateAllPieces a xs)) xs

-- returns all pieces that a piece is threatening
threatening :: Piece -> AllPieces -> [Piece]
threatening a xs = getPiecesFromMoves (getPos a) (legalMoves a xs) xs

-- get a list of pieces from a list of moves and a starting position
getPiecesFromMoves :: Pos -> [Move] -> AllPieces -> [Piece]
getPiecesFromMoves a [] xs = []
getPiecesFromMoves a ms xs | not (null y) = head y : getPiecesFromMoves a (tail ms) xs
                           | otherwise = getPiecesFromMoves a (tail ms) xs
                           where
                             y = findPiece (getTarget a (head ms)) xs

-- returns whether the king is in check.
isKingInCheck :: Piece -> AllPieces -> Bool
isKingInCheck a b | null (threatenedBy a b) = False
                  | otherwise = True

-- removes a piece from the board
takePiece :: Piece -> AllPieces -> AllPieces
takePiece (p, col, pos, mc) d = (p, col, (-1,-1), 0) : removePiece (p, col, pos, mc) d

--move piece
movePiece :: Piece -> Move -> AllPieces -> AllPieces
movePiece a b c | getPieceType a  == King && (b == (0,2) || b == (0,-2)) && validCastle a b c = executeCastle a b c
                | isValidEnPassant a b c = captureEnPassant a b c
                | isValidPromotion a b c = promotePawn a b c
                | isValidMove a b c && not (isKingInCheck (King, (getColour a), king, getMovecount a) c) = executeMove a b c
                | otherwise = c
                  where king = findKing (getColour a) c

-- returns whether a king has moved
--hasKingNotMoved :: Colour -> Bool -> Bool -> Bool
--hasKingNotMoved White a b = not b
--hasKingNotMoved Black a b = not a

--execute move
executeMove :: Piece -> Move -> AllPieces -> AllPieces
executeMove a b c | not (isTargetEnemy a b c) = updatePosition a b : removePiece a c
                  | otherwise = takePiece y z
                              where
                                    y = head (findPiece (getTarget (getPos a) b) c)
                                    z = updatePosition a b : removePiece a c



-- writes a move to the pgn file WORKING
writeMove :: Piece -> Move -> IO ()
writeMove (piece,colour,(m,n),mc) (rows,cols) = do copyFile "movelist.pgn" "movelistTemp.pgn"
                                                   appendFile "movelistTemp.pgn" ((show piece) ++ ";" ++ (show colour) ++ ";" ++ (show m) ++ ";" ++ (show n) ++ ";" ++ (show mc) ++ ";" ++ (show rows) ++ ";" ++ (show cols) ++ (show mc) ++ "\n")
                                                   removeFile "movelist.pgn"
                                                   renameFile "movelistTemp.pgn" "movelist.pgn"

-- makes a move and writes it to the pgn. returns AllPieces WORKING
makeProperMove :: Piece -> Move -> AllPieces -> IO AllPieces
makeProperMove a b cs = do writeMove a b
                           return (movePiece a b cs)

-- returns whether the king has moved before or not
readMoveList :: IO String
readMoveList = do x <- readFile "movelist.pgn"
                  let pureX = read x
                  return pureX

testMoveList :: String -> String
testMoveList a = a

-- returns King's side castle for either colour
getKingsCastle :: Colour -> Pos
getKingsCastle White = (7,7)
getKingsCastle Black = (0,7)

-- returns Queen's side castle for either colour
getQueensCastle :: Colour -> Pos
getQueensCastle White = (0,7)
getQueensCastle Black = (0,0)

-- returns whether a king and the revelant castle has moved or not. True = kings side castle
possibleToCastle :: Colour -> Bool -> AllPieces -> Bool
possibleToCastle c True ps = not (null (findPiece (getKingsCastle c) ps)) && getMovecount (head (findPiece (findKing c ps) ps)) == 0 && getMovecount (head (findPiece (getKingsCastle c) ps)) == 0
possibleToCastle c False ps = not (null (findPiece (getQueensCastle c) ps)) && getMovecount (head (findPiece (findKing c ps) ps)) == 0 && getMovecount (head (findPiece (getQueensCastle c) ps)) == 0


--returns whether a castle is valid or not
validCastle :: Piece -> Move -> AllPieces -> Bool
validCastle a (0,2) b  = isStraightMovePathEmpty (getPos a) (0,2) b && possibleToCastle (getColour a) True b
validCastle a (0,-2) b = isStraightMovePathEmpty (getPos a) (0,-3) b && possibleToCastle (getColour a) False b

-- executes a castle move -- WORKING
executeCastle :: Piece -> Move -> AllPieces -> AllPieces
executeCastle a (0,2) b = executeMove a (0,2) (executeMove (head (findPiece (getKingsCastle (getColour a)) b)) (0,-2) b)
executeCastle a (0,-2) b = executeMove a (0,-2) (executeMove (head (findPiece (getQueensCastle (getColour a)) b)) (0,3) b)

-- return a list of legal moves that a knight can make
legalKnightMoves :: Piece -> AllPieces -> [Move]
legalKnightMoves a b = [ x | x <- y, isKnightValidMove a x b ]
                     where
                         z = [-2,-1,1,2]
                         y = [ (m,n) | m <- z, n <- z, isLShaped (m,n)]

-- return a list of legal moves that a rook can make -- efficiency vs concised code?
                                                     -- 3 options shown for this function
                                                     -- recursive method more efficient bc it can stop searching one direction as soon as it encounters an obstruction?
                                                     -- using :set +s in GHCi, i can see there is a 0.01s difference on my laptop between definitions 1 and 2 for this function (however this isnt evaluating the final compiled version, only the interpreted version)
                                                     -- currently i think we should leave it in its most consised and readable form and think about optimisations when we have a compiled executable
                                                     -- definition 1 uses less memory, but it is slower by 0.01s (might add up over lots of calls)
legalRookMoves :: Piece -> AllPieces -> [Move]
legalRookMoves a b = [ (m,n) | m <- [-7..7], n <- [-7..7], isRookValidMove a (m,n) b ]
--legalRookMoves a b = [ x | x <- y, isRookValidMove a x b ]
--                   where
--                       y = [ (m,n) | m <- [-7..7], n <- [-7..7], isStraightMove (m,n) ]
--                       --z = [ (m,0) | m <- [-7..7] ]
--                       --v = [ (0,n) | n <- [-7..7] ]
--                       --y = z++v

-- return a list of legal moves for a bishop -- same questions as legalRookMoves
legalBishopMoves :: Piece -> AllPieces -> [Move]
legalBishopMoves a b = [ (m,n) | m <- [-7..7], n <- [-7..7], isBishopValidMove a (m,n) b ]
--legalBishopMoves a b = [ x | x <- y , isBishopValidMove a x b ]
--                     where
--                         y = [ (m,n) | m <- [-7..7], n <- [-7..7], isDiagonal (m,n) ]

-- returns a list of legal moves for a queen
legalQueenMoves :: Piece -> AllPieces -> [Move]
legalQueenMoves a b = (legalBishopMoves a b) ++ (legalRookMoves a b)

-- returns a list of legal moves for a pawn
legalPawnMoves :: Piece -> AllPieces -> [Move]
legalPawnMoves a b = [ (m,n) | m <- [-2..2], n <- [-1..1], isPawnValidMove a (m,n) b || isValidEnPassant a (m,n) b]

-- returns a list of legal moves for a king
legalKingMoves :: Piece -> AllPieces -> [Move]
legalKingMoves a b = [(m,n) | m <- [-1..1], n <- [-1..1], validKingMove a (m,n) b]

-- returns a list of legal moves for a piece
legalMoves :: Piece -> AllPieces -> [Move]
legalMoves (Pawn, col, pos, mc) x   = legalPawnMoves (Pawn, col, pos, mc) x
legalMoves (Knight, col, pos, mc) x = legalKnightMoves (Knight, col, pos, mc) x
legalMoves (Bishop, col, pos, mc) x = legalBishopMoves (Bishop, col, pos, mc) x
legalMoves (Rook, col, pos, mc) x   = legalRookMoves (Rook, col, pos, mc) x
legalMoves (Queen, col, pos, mc) x  = legalQueenMoves (Queen, col, pos, mc) x
legalMoves (King, col, pos, mc) x = legalKingMoves (King, col, pos, mc) x

-- returns a list of positions the pawn is controlling
pawnControlledSquares :: Piece -> [Pos]
pawnControlledSquares a = [ getTarget (getPos a) (m,n) | m <- [-1,1], n <- [-1,1], isPawnCapture a (m,n) ]


-- returns a list containg all the surrounding friendly pieces from a position
surroundingPieces :: Colour -> [Pos] -> AllPieces -> [Piece]
surroundingPieces a [] b = []
surroundingPieces a xs b | not (null p) && a ==  getColour (head p) = head p : surroundingPieces a (tail xs) b
                         | otherwise = surroundingPieces a (tail xs) b
                           where p = findPiece (head xs) b

isIntOnBoard :: Int -> Bool
isIntOnBoard a | a < 0 || a > 7 = False
               | otherwise = True

getSurroundingPos :: Pos -> [Pos]
getSurroundingPos (m,n) = [ (row,col) | row <- [m-1..m+1], col <- [n-1..n+1], isIntOnBoard row, isIntOnBoard col, (row,col) /= (m,n)]
