module Util where

import           Data.List
import           Debug
import           Debug
import           Init
import           System.Directory
import           TypeDefs
import           Debug.Trace

-- GETTERS

-- returns the column of a position
getColumn :: Pos -> Int
getColumn (_,col) = col

-- returns the row of a position
getRow :: Pos -> Int
getRow (row,_) = row

-- returns the colour of a piece
getColour :: Piece -> Colour
getColour (_,colour,_,_) = colour

-- returns the position of a piece
getPos :: Piece -> Pos
getPos (_,_,pos,_) = pos

-- returns the type of a piece
getPieceType :: Piece -> PieceType
getPieceType (x,_,_,_) = x

--returns a list with a given piece removed
removePiece :: Piece -> [Piece] -> [Piece]
removePiece pieceToRemove ps = [ p | p <- ps, p /= pieceToRemove]

-- returns what move was made based on starting position and final position
moveMade :: Pos -> Pos -> Move
moveMade (a,b) (c,d) = (c - a, d - b)

-- returns a piece with an updated position
updatePosition :: Piece -> Move -> Piece
updatePosition (p,colour,pos,mc) move = (p, colour, (getTarget pos move), mc+1)

-- returns the starting position of a move
--getStart :: Move -> Pos
--getStart (x,_,_) = getPos x

-- returns the target position for a move WORKING
getTarget :: Pos -> Move -> Pos
getTarget (a, b) (c, d) = (a+c, b+d)

-- returns the piece that is on a square in a list (empty list if no piece there) WORKING
findPiece :: Pos -> AllPieces -> [Piece]
findPiece pos ps = [x | x <- ps, getPos x == pos]

-- returns the inversion of a colour
invertColour :: Colour -> Colour
invertColour White = Black
invertColour Black = White

-- invert piece colour
invertPieceColour :: Piece -> Piece
invertPieceColour (a,colour,pos,mc) = (a, invertColour colour, pos, mc)


-- gets the ammount of moves a piece has made
getMovecount :: Piece -> Movecount
getMovecount (_,_,_,mc) = mc

-- UTILITIES AND RULES

-- returns true if there is no piece on pos WORKING
isEmpty :: Pos -> AllPieces -> Bool
isEmpty pos ps = (findPiece pos ps) == []

-- returns true if the two pieces are enemies WORKING
isEnemy :: Piece -> Piece -> Bool
isEnemy p z = getColour p /= getColour z

-- returns true if the two piece are friendly
isFriendly :: Piece -> Piece -> Bool
isFriendly p z = getColour p == getColour z

-- returns whether a move will mean the piece ends up off of the board or not -- WORKING
isOnBoard :: Piece -> Move -> Bool
isOnBoard p move = row >= 0 && row <= 7 && col >= 0 && col <= 7
                where
                    row = getRow (getTarget (getPos p) move)
                    col = getColumn (getTarget (getPos p) move)

-- returns whether a square is not occupied by a friendly piece -- NOT WORKING: needs to prevent the king from being in check after the move has been made
-- recursion problem: calls itsself through isKingInCheck
isValidTarget :: Piece -> Move -> AllPieces -> Bool
isValidTarget p move ps = ((isEmpty (getTarget (getPos p) move) ps) || (isEnemy p z)) && isOnBoard p move
                        where
                          z = head (findPiece (getTarget (getPos p) move) ps)


-- returns whether a square is occupied by an enemy -- WORKING
isTargetEnemy :: Piece -> Move -> AllPieces -> Bool
isTargetEnemy p move ps = not (isEmpty (getTarget (getPos p) move) ps) && isEnemy p z -- checks whether square is empty to prevent empty list being passed to head and causing an error - ray
                        where
                          z = head(findPiece (getTarget (getPos p) move) ps)

-- get an int closer to 0
closerToZero :: Int -> Int
closerToZero a | a < 0 = a + 1
               | a > 0 = a - 1 -- added this line so -1 isnt returned if a is 0 - from ray
               | otherwise = a

-- decrease the value of a move by 1 closer to the original position
decreaseDiagonalMove :: Move -> Move
decreaseDiagonalMove (row,col) = (closerToZero row, closerToZero col)


--checks to see if a piece can move along a diagonal line without hitting any pieces.
isDiagonalMovePathEmpty :: Pos -> Move -> AllPieces -> Bool
isDiagonalMovePathEmpty pos (0,0) ps = True
isDiagonalMovePathEmpty pos move ps = isEmpty (getTarget pos move) ps && isDiagonalMovePathEmpty pos m2 ps
                                    where
                                      m2 = decreaseDiagonalMove move

-- checks to see if a piece can move along a straight line without hitting any pieces.
isStraightMovePathEmpty :: Pos -> Move -> AllPieces -> Bool
isStraightMovePathEmpty pos (0,0) ps = True
isStraightMovePathEmpty pos (0,col) ps = isEmpty (getTarget pos (0,col)) ps && isStraightMovePathEmpty pos (0,c2) ps
                                       where
                                         c2 = closerToZero col
isStraightMovePathEmpty pos (row,0) ps = isEmpty (getTarget pos (row,0)) ps && isStraightMovePathEmpty pos (r2,0) ps
                                       where
                                         r2 = closerToZero row

--returns whether a pawn move is a capture -- WORKING (i think)
isPawnCapture :: Piece -> Move -> Bool
isPawnCapture (_,Black,_,_) (row,col) = row == 1 && abs col == 1
isPawnCapture (_,White,_,_) (row,col) = row == -1 && abs col == 1

--returns whether a pawn move is a regular pawn move -- swapped a and b (row,column) WORKING
isBasicPawnMove :: Piece -> Move -> AllPieces-> Bool
isBasicPawnMove (_,Black,(1,n),_) (row,col)  ps = ((row == 2 && (isStraightMovePathEmpty (1,n) (row,0) ps)) || row == 1) && col == 0
isBasicPawnMove (_,White,(6,n),_) (row,col)  ps = ((row == -2 && (isStraightMovePathEmpty (6,n) (row,0) ps)) || row == -1) && col == 0
isBasicPawnMove (_,Black,_,_) (row,col)      ps = row == 1 && col == 0
isBasicPawnMove (_,White,_,_) (row,col)      ps = row == -1 && col == 0

-- returns whether a pawn move is a promotion
isValidPromotion :: Piece -> Move -> AllPieces -> Bool
isValidPromotion p move ps = getPieceType p == Pawn && isPawnValidMove p move ps && getRow (getTarget (getPos p) move) == e
                          where
                              e = if getColour p == White then 0 else 7

-- changes a piece to a queen
upgradeToQueen :: Piece -> AllPieces -> AllPieces
upgradeToQueen (piece,colour,(m,n),mc) ps = (Queen, colour, (m,n), mc) : (removePiece (piece, colour, (m,n), mc) ps)

-- promotes a pawn
promotePawn :: Piece -> Move -> AllPieces -> AllPieces
promotePawn p move ps = upgradeToQueen piece board
                      where
                        board = executeMove p move ps
                        piece = head (findPiece (getTarget (getPos p) move) board)


-- returns whether a move is in a straight line or not
isStraightMove :: Move -> Bool
isStraightMove (row,col) = (row == 0 || col == 0)

-- return whether a move is a diagonal
isDiagonal :: Move -> Bool
isDiagonal (row,col) = abs row == abs col

-- returns whether a move is L-shaped.
isLShaped :: Move -> Bool
isLShaped (row,col) = (abs row == 2 && abs col == 1) || (abs row == 1 && abs col == 2)

-- returns whether a pawn move is valid
isPawnValidMove :: Piece -> Move -> AllPieces -> Bool
isPawnValidMove p move ps = isValidTarget p move ps && ( (isEmpty (getTarget (getPos p) move) ps && isBasicPawnMove p move ps && isStraightMovePathEmpty (getPos p) move ps) || (isTargetEnemy p move ps && isPawnCapture p move))

-- returns whether a move is a valid en passant move
isValidEnPassant :: Piece -> Move -> AllPieces -> Bool
isValidEnPassant a (m,n) ps = isPawnCapture a (m,n) && getRow (getPos a) == r && not (isEmpty (getTarget (getPos a) (0,n)) ps) && p == (Pawn, invertColour (getColour a), (getRow (getPos a), getColumn (getPos a) + n), 1)
                              where
                                  p = head (findPiece (getTarget (getPos a) (0,n)) ps)
                                  r = if getColour a == White then 3 else 4

-- moves the pawn and removes the adjacent enemy pawn
-- note this can produce an error if used when the move is not a validEnPassant cus of the use of head
captureEnPassant :: Piece -> Move -> AllPieces -> AllPieces
captureEnPassant p (m,n) ps = executeMove p (m,n) (removePiece (head (findPiece ( getTarget (getPos p) (0,n) ) ps )) ps)

-- returns whether a knight move is valid -- WORKING (i think. needs thorough testing)
isKnightValidMove :: Piece -> Move -> AllPieces -> Bool
isKnightValidMove p move ps = isValidTarget p move ps && isLShaped move

-- returns whether a bishop move is valid
isBishopValidMove :: Piece -> Move -> AllPieces -> Bool
isBishopValidMove p move ps = isDiagonal move && isValidTarget p move ps && isDiagonalMovePathEmpty (getPos p) (decreaseDiagonalMove move) ps

isRookValidMove :: Piece -> Move -> AllPieces -> Bool
isRookValidMove p (0,col) ps = isValidTarget p (0,col) ps && isStraightMovePathEmpty (getPos p) (0,closerToZero col) ps
isRookValidMove p (row,0) ps = isValidTarget p (row,0) ps && isStraightMovePathEmpty (getPos p) (closerToZero row,0) ps
isRookValidMove p move ps = False

-- returns whether a queen move is valid
isQueenValidMove :: Piece -> Move -> AllPieces -> Bool
isQueenValidMove p move ps = isRookValidMove p move ps || isBishopValidMove p move ps

-- returns the position of the king WORKING
findKing :: Colour -> AllPieces -> Pos
findKing colour ps | null l = trace (show colour ++ "\n" ++ show ps) (-7,-7)
                   | otherwise = head l
                   where
                     l = [(x,y) | (t, c, (x,y), m) <- ps, t == King, c == colour, x > -1, y > -1, x < 8, y < 8 ]


-- returns whether a king move is valid WORKING
validKingMove :: Piece -> Move -> AllPieces -> Bool
validKingMove p (m,n) ps | abs n == 2 = validCastle p (m,n) ps
                         | otherwise = (abs m <= 1 && abs n <= 1 ) && isValidTarget p (m,n) ps && (x > 1 || y > 1)
                                    where
                                        x = abs ((getColumn (getTarget (getPos p) (m,n)) - (getColumn (findKing (invertColour (getColour p)) ps))))
                                        y = abs ((getRow (getTarget (getPos p) (m,n)) - (getRow (findKing (invertColour (getColour p)) ps))))


-- returns whether a move is valid
isValidMove :: Piece -> Move -> AllPieces -> Bool
isValidMove (Pawn, col, pos, mc) move ps   = isPawnValidMove (Pawn, col, pos, mc) move ps || isValidEnPassant (Pawn,col,pos,mc) move ps || isValidPromotion (Pawn,col,pos,mc) move ps
isValidMove (Knight, col, pos, mc) move ps = isKnightValidMove (Knight, col, pos, mc) move ps
isValidMove (Bishop, col, pos, mc) move ps = isBishopValidMove (Bishop, col, pos, mc) move ps
isValidMove (Rook, col, pos, mc) move ps   = isRookValidMove (Rook, col, pos, mc) move ps
isValidMove (Queen, col, pos, mc) move ps  = isQueenValidMove (Queen, col, pos, mc) move ps
isValidMove (King, col, pos, mc) move ps   = validKingMove (King, col, pos, mc) move ps

-- returns a list of the pieces which can capture piece a
threatenedBy :: Piece -> AllPieces -> [Piece]
threatenedBy p ps = [ x | x <- ps, isValidMove x (m - getRow (getPos x), n - getColumn (getPos x)) ps ]
                  where
                      m = getRow (getPos p)
                      n = getColumn (getPos p)

-- changes all pieces to reflect the inverted colour.
updateAllPieces :: Piece -> AllPieces -> [Piece]
updateAllPieces p ps = invertPieceColour p : removePiece p ps

-- returns a list of the pieces which
protectedBy :: Piece -> AllPieces -> [Piece]
protectedBy p ps = threatenedBy (invertPieceColour p) (updateAllPieces p ps)

-- returns all pieces that a piece is protecting
protecting :: Piece -> AllPieces -> [Piece]
protecting (King,colour,pos,mc) ps = surroundingPieces colour (getSurroundingPos pos) ps
protecting p ps = getPiecesFromMoves (getPos p) (legalMoves (invertPieceColour p) (updateAllPieces p ps)) ps

-- returns all pieces that a piece is threatening
threatening :: Piece -> AllPieces -> [Piece]
threatening p ps = getPiecesFromMoves (getPos p) (legalMoves p ps) ps

-- get a list of pieces from a list of moves and a starting position
getPiecesFromMoves :: Pos -> [Move] -> AllPieces -> [Piece]
getPiecesFromMoves pos [] ps = []
getPiecesFromMoves pos ms ps | not (null y) = head y : getPiecesFromMoves pos (tail ms) ps
                             | otherwise = getPiecesFromMoves pos (tail ms) ps
                               where
                                 y = findPiece (getTarget pos (head ms)) ps

-- returns whether the king is in check.
isKingInCheck :: Piece -> AllPieces -> Bool
isKingInCheck p ps | null (threatenedBy p ps) = False
                   | otherwise = True

-- removes a piece from the board
takePiece :: Piece -> AllPieces -> AllPieces
takePiece (p, colour, pos, mc) ps = (p, colour, (-1,-1), 0) : removePiece (p, colour, pos, mc) ps

--move piece
movePiece :: Piece -> Move -> AllPieces -> AllPieces
movePiece p move ps | isValidMove p move ps && not (willKingBeInCheck p move ps) = executeMove p move ps
                    | otherwise = ps


-- returns whether a king has moved
--hasKingNotMoved :: Colour -> Bool -> Bool -> Bool
--hasKingNotMoved White a b = not b
--hasKingNotMoved Black a b = not a

--execute move
executeMove :: Piece -> Move -> AllPieces -> AllPieces
executeMove p move ps | not (isTargetEnemy p move ps) = updatePosition p move : removePiece p ps
                      | otherwise = takePiece y z
                                where
                                    y = head (findPiece (getTarget (getPos p) move) ps)
                                    z = updatePosition p move : removePiece p ps



-- writes a move to the pgn file WORKING
writeMove :: Piece -> Move -> IO ()
writeMove (piece,colour,(m,n),mc) (rows,cols) = do copyFile "movelist.pgn" "movelistTemp.pgn"
                                                   appendFile "movelistTemp.pgn" ((show piece) ++ ";" ++ (show colour) ++ ";" ++ (show m) ++ ";" ++ (show n) ++ ";" ++ (show mc) ++ ";" ++ (show rows) ++ ";" ++ (show cols) ++ (show mc) ++ "\n")
                                                   removeFile "movelist.pgn"
                                                   renameFile "movelistTemp.pgn" "movelist.pgn"

-- makes a move and writes it to the pgn. returns AllPieces WORKING
makeProperMove :: Piece -> Move -> AllPieces -> IO AllPieces
makeProperMove p move ps = do writeMove p move
                              return (movePiece p move ps)

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
validCastle p (0,2) ps  = isStraightMovePathEmpty (getPos p) (0,2) ps && possibleToCastle (getColour p) True ps
validCastle p (0,-2) ps = isStraightMovePathEmpty (getPos p) (0,-3) ps && possibleToCastle (getColour p) False ps
validCastle _ _ _ = False

-- executes a castle move -- WORKING
executeCastle :: Piece -> Move -> AllPieces -> AllPieces
executeCastle p (0,2) ps = executeMove p (0,2) (executeMove (head (findPiece (getKingsCastle (getColour p)) ps)) (0,-2) ps)
executeCastle p (0,-2) ps = executeMove p (0,-2) (executeMove (head (findPiece (getQueensCastle (getColour p)) ps)) (0,3) ps)

-- return a list of legal moves that a knight can make
legalKnightMoves :: Piece -> AllPieces -> [Move]
legalKnightMoves p ps = [ x | x <- y, isKnightValidMove p x ps, targetNotKing p x ps, not (willKingBeInCheck p x ps) ]
                     where
                         z = [-2,-1,1,2]
                         y = [ (m,n) | m <- z, n <- z, isLShaped (m,n)]

-- returns whether teh king will be in check after a move is made
willKingBeInCheck :: Piece -> Move -> AllPieces -> Bool
willKingBeInCheck p m ps = isKingInCheck k n
                           where
                               n = executeMove p m ps
                               k = head (findPiece (findKing (getColour p) n) n)

-- return a list of legal moves that a rook can make -- efficiency vs concised code?
                                                     -- 3 options shown for this function
                                                     -- recursive method more efficient bc it can stop searching one direction as soon as it encounters an obstruction?
                                                     -- using :set +s in GHCi, i can see there is a 0.01s difference on my laptop between definitions 1 and 2 for this function (however this isnt evaluating the final compiled version, only the interpreted version)
                                                     -- currently i think we should leave it in its most consised and readable form and think about optimisations when we have a compiled executable
                                                     -- definition 1 uses less memory, but it is slower by 0.01s (might add up over lots of calls)
legalRookMoves :: Piece -> AllPieces -> [Move]
legalRookMoves p ps = [ (m,n) | m <- [-7..7], n <- [-7..7], targetNotKing p (m,n) ps, isRookValidMove p (m,n) ps, not (willKingBeInCheck p (m,n) ps) ]
--legalRookMoves a b = [ x | x <- y, isRookValidMove a x b ]
--                   where
--                       y = [ (m,n) | m <- [-7..7], n <- [-7..7], isStraightMove (m,n) ]
--                       --z = [ (m,0) | m <- [-7..7] ]
--                       --v = [ (0,n) | n <- [-7..7] ]
--                       --y = z++v


-- return a list of legal moves for a bishop -- same questions as legalRookMoves
legalBishopMoves :: Piece -> AllPieces -> [Move]
legalBishopMoves p ps = [ (m,n) | m <- [-7..7], n <- [-7..7], targetNotKing p (m,n) ps, isBishopValidMove p (m,n) ps, not (willKingBeInCheck p (m,n) ps) ]
--legalBishopMoves a b = [ x | x <- y , isBishopValidMove a x b ]
--                     where
--                         y = [ (m,n) | m <- [-7..7], n <- [-7..7], isDiagonal (m,n) ]

-- returns a list of legal moves for a queen
legalQueenMoves :: Piece -> AllPieces -> [Move]
legalQueenMoves p ps = (legalBishopMoves p ps) ++ (legalRookMoves p ps)

-- returns a list of legal moves for a pawn
legalPawnMoves :: Piece -> AllPieces -> [Move]
legalPawnMoves p ps = [ (m,n) | m <- [-2..2], n <- [-1..1], targetNotKing p (m,n) ps, isPawnValidMove p (m,n) ps || isValidEnPassant p (m,n) ps || isValidPromotion p (m,n) ps, not (willKingBeInCheck p (m,n) ps) ]

-- returns a list of legal moves for a king
legalKingMoves :: Piece -> AllPieces -> [Move]
legalKingMoves p ps = [(m,n) | m <- [-1..1], n <- [-2..2], targetNotKing p (m,n) ps, validKingMove p (m,n) ps, not (willKingBeInCheck p (m,n) ps)]

-- returns a list of legal moves for a piece
legalMoves :: Piece -> AllPieces -> [Move]
legalMoves (Pawn, col, pos, mc) x   = legalPawnMoves (Pawn, col, pos, mc) x
legalMoves (Knight, col, pos, mc) x = legalKnightMoves (Knight, col, pos, mc) x
legalMoves (Bishop, col, pos, mc) x = legalBishopMoves (Bishop, col, pos, mc) x
legalMoves (Rook, col, pos, mc) x   = legalRookMoves (Rook, col, pos, mc) x
legalMoves (Queen, col, pos, mc) x  = legalQueenMoves (Queen, col, pos, mc) x
legalMoves (King, col, pos, mc) x = legalKingMoves (King, col, pos, mc) x

-- returns whether the target square is a king
targetNotKing :: Piece -> Move -> AllPieces -> Bool
targetNotKing p m ps = null t || getPieceType (head t) /= King
                       where
                           t = findPiece (getTarget (getPos p) m) ps

-- returns a list of positions the pawn is controlling
pawnControlledSquares :: Piece -> [Pos]
pawnControlledSquares p = [ getTarget (getPos p) (m,n) | m <- [-1,1], n <- [-1,1], isPawnCapture p (m,n) ]


-- returns a list containg all the surrounding friendly pieces from a position
surroundingPieces :: Colour -> [Pos] -> AllPieces -> [Piece]
surroundingPieces colour [] ps = []
surroundingPieces colour xs ps | not (null p) && colour ==  getColour (head p) = head p : surroundingPieces colour (tail xs) ps
                               | otherwise = surroundingPieces colour (tail xs) ps
                                           where p = findPiece (head xs) ps

isIntOnBoard :: Int -> Bool
isIntOnBoard a | a < 0 || a > 7 = False
               | otherwise = True

getSurroundingPos :: Pos -> [Pos]
getSurroundingPos (m,n) = [ (row,col) | row <- [m-1..m+1], col <- [n-1..n+1], isIntOnBoard row, isIntOnBoard col, (row,col) /= (m,n)]
