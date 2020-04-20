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
getColour (_,x,_) = x

-- returns the position of a piece
getPos :: Piece -> Pos
getPos (_,_,x) = x

getPieceType :: Piece -> PieceType
getPieceType (x,_,_) = x

--returns a list with a given piece removed
removePiece :: Piece -> [Piece] -> [Piece]
removePiece pieceToRemove xs = [ x | x <- xs, x /= pieceToRemove]

-- returns a piece with an updated position
updatePosition :: Piece -> Move -> Piece
updatePosition (a,b,c) d = (a, b, (getTarget c d))

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



-- UTILITIES AND RULES

-- returns true if there is no piece on pos WORKING
isEmpty :: Pos -> AllPieces -> Bool
isEmpty a b = (findPiece a b) == []

-- returns true if the two pieces are enemies WORKING
isEnemy :: Piece -> Piece -> Bool
isEnemy a b = getColour a /= getColour b

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
isPawnCapture (_,Black,_) (a,b) = a == 1 && abs b == 1
isPawnCapture (_,White,_) (a,b) = a == -1 && abs b == 1
-- change made - ray
--isPawnCapture (_,Black,_) (a,b) = b == 1 && abs a == 1
--isPawnCapture(_,White,_) (a,b)  = b == -1 && abs a == 0

--returns whether a pawn move is a regular pawn move -- swapped a and b (row,column) WORKING
isBasicPawnMove :: Piece -> Move -> Bool
isBasicPawnMove (_,Black,(1,_)) (a,b) = (a == 2 || a == 1) && b == 0
isBasicPawnMove (_,White,(6,_)) (a,b) = (a == -2 || a == -1) && b == 0
isBasicPawnMove (_,Black,_) (a,b)     = a == 1 && b == 0
isBasicPawnMove (_,White,_) (a,b)     = a == -1 && b == 0

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
isPawnValidMove a b c = isValidTarget a b c && ( (isEmpty (getTarget (getPos a) b) c && isBasicPawnMove a b) || (isTargetEnemy a b c && isPawnCapture a b))

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
findKing a b = head [p | (t, c, p) <- b, t == King, c == a]

-- returns whether a king move is valid WORKING
validKingMove :: Piece -> Move -> AllPieces -> Bool
validKingMove a (m,n) b = (abs m <= 1 && abs n <= 1 ) && isValidTarget a (m,n) b && (x > 1 || y > 1)
                          where
                              x = abs ((getColumn (getTarget (getPos a) (m,n)) - (getColumn (findKing (invertColour (getColour a)) b))))
                              y = abs ((getRow (getTarget (getPos a) (m,n)) - (getRow (findKing (invertColour (getColour a)) b))))


-- returns whether a move is valid
isValidMove :: Piece -> Move -> AllPieces -> Bool
isValidMove (Pawn, col, pos) x y   = isPawnValidMove (Pawn, col, pos) x y
isValidMove (Knight, col, pos) x y = isKnightValidMove (Knight, col, pos) x y
isValidMove (Bishop, col, pos) x y = isBishopValidMove (Bishop, col, pos) x y
isValidMove (Rook, col, pos) x y   = isRookValidMove (Rook, col, pos) x y
isValidMove (Queen, col, pos) x y  = isQueenValidMove (Queen, col, pos) x y
isValidMove (King, col, pos) x y   = validKingMove (King, col, pos) x y

-- returns a list of the pieces which can capture piece a
threatenedBy :: Piece -> AllPieces -> [Piece]
threatenedBy a b = [ x | x <- b, isValidMove x (m - getRow (getPos x), n - getColumn (getPos x)) b ]
                  where
                      m = getRow (getPos a)
                      n = getColumn (getPos a)

-- returns whether the king is in check.
isKingInCheck :: Piece -> AllPieces -> Bool
isKingInCheck a b | null (threatenedBy a b) = False
                  | otherwise = True

-- removes a piece from the board
takePiece :: Piece -> AllPieces -> AllPieces
takePiece (a,b,c) d = (a,b,(-1,-1)) : removePiece (a,b,c) d

--move piece
movePiece :: Piece -> Move -> Bool -> Bool -> AllPieces -> AllPieces
movePiece a b blackKing whiteKing c | getPieceType a  == King && (b == (0,2) || b == (0,-2)) && validCastle a b c && hasKingNotMoved (getColour a) blackKing whiteKing = executeCastle a b c
                                    | isValidMove a b c && not (isKingInCheck (King, (getColour a), king) c) = executeMove a b c
                                    | otherwise = c
                                      where king = findKing (getColour a) c

-- returns whether a king has moved
hasKingNotMoved :: Colour -> Bool -> Bool -> Bool
hasKingNotMoved White a b = not b
hasKingNotMoved Black a b = not a

--execute move
executeMove :: Piece -> Move -> AllPieces -> AllPieces
executeMove a b c | not (isTargetEnemy a b c) = updatePosition a b : removePiece a c
                  | otherwise = takePiece y z
                              where
                                    y = head (findPiece (getTarget (getPos a) b) c)
                                    z = updatePosition a b : removePiece a c



-- writes a move to the pgn file WORKING
writeMove :: Piece -> Move -> IO ()
writeMove a b = do copyFile "movelist.pgn" "movelistTemp.pgn"
                   appendFile "movelistTemp.pgn" (((show a) ++ ";" ++ (show b)) ++ "\n")
                   removeFile "movelist.pgn"
                   renameFile "movelistTemp.pgn" "movelist.pgn"

-- makes a move and writes it to the pgn. returns AllPieces WORKING
makeProperMove :: Piece -> Move -> Bool -> Bool -> AllPieces -> IO AllPieces
makeProperMove a b blackKing whiteKing cs = do writeMove a b
                                               return (movePiece a b blackKing whiteKing cs)

-- returns whether the king has moved before or not
readMoveList :: IO String
readMoveList = do x <- readFile "movelist.pgn"
                  let pureX = read x
                  return pureX

testMoveList :: String -> String
testMoveList a = a

-- returns King's side castle for either colour
getKingsCastle :: Colour -> Piece
getKingsCastle White = (Rook, White, (7,7))
getKingsCastle Black = (Rook, Black, (0,7))

-- returns Queen's side castle for either colour
getQueensCastle :: Colour -> Piece
getQueensCastle White = (Rook, White, (7,0))
getQueensCastle Black = (Rook, Black, (0,0))

--returns whether a castle is valid or not
validCastle :: Piece -> Move -> AllPieces -> Bool
validCastle a (0,2) b  = isStraightMovePathEmpty (getPos a) (0,2) b
validCastle a (0,-2) b = isStraightMovePathEmpty (getPos a) (0,-3) b

-- executes a castle move -- WORKING
executeCastle :: Piece -> Move -> AllPieces -> AllPieces
executeCastle a (0,2) b = executeMove a (0,2) (executeMove (getKingsCastle (getColour a)) (0,-2) b)
executeCastle a (0,-2) b = executeMove a (0,-2) (executeMove (getQueensCastle (getColour a)) (0,3) b)

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
legalPawnMoves a b = [ (m,n) | m <- [-2..2], n <- [-1..1], isPawnValidMove a (m,n) b ]

-- returns a list of legal moves for a piece
legalMoves :: Piece -> AllPieces -> [Move]
legalMoves (Pawn, col, pos) x   = legalPawnMoves (Pawn, col, pos) x
legalMoves (Knight, col, pos) x = legalKnightMoves (Knight, col, pos) x
legalMoves (Bishop, col, pos) x = legalBishopMoves (Bishop, col, pos) x
legalMoves (Rook, col, pos) x   = legalRookMoves (Rook, col, pos) x
legalMoves (Queen, col, pos) x  = legalQueenMoves (Queen, col, pos) x

-- returns a list of positions the pawn is controlling
pawnControlledSquares :: Piece -> [Pos]
pawnControlledSquares a = [ getTarget (getPos a) (m,n) | m <- [-1,1], n <- [-1,1], isPawnCapture a (m,n) ]
