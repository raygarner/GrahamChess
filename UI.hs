module UI where

import TypeDefs
import Init
import Util
import Data.Char
--import Text.Show.Unicode

calcPos :: Int -> Pos
calcPos a = (a `div` 8, a `mod` 8)


pieceChar :: Piece -> Char
pieceChar (Pawn,Black,_,_) = '♙'
pieceChar (Pawn,White,_,_) = '♟'
pieceChar (Knight,White,_,_) = '♞'
pieceChar (Knight,Black,_,_) = '♘'
pieceChar (Bishop,Black,_,_) ='♗'
pieceChar (Bishop,White,_,_) = '♝'
pieceChar (Rook,Black,_,_) = '♖'
pieceChar (Rook,White,_,_) = '♜'
pieceChar (Queen,Black,_,_) = '♕'
pieceChar (Queen,White,_,_) = '♛'
pieceChar (King,Black,_,_) = '♔'
pieceChar (King,White,_,_) = '♚'


-- white pieces shown in upper case, black pieces in lower case
colourCase :: Colour -> Char -> Char
colourCase c s | c == White = toUpper s
               | otherwise  = toLower s

printBoard :: Int -> AllPieces -> IO ()
printBoard 64 ps = do printCols
                      printBoard 65 ps
printBoard 65 ps = do printCapturedPieces Black ps
printBoard (-1) ps = do printCapturedPieces White ps
                        printBoard 0 ps
printBoard n ps = do if (getColumn (calcPos n) == 0) then
                         do
                           putStr (show (getRow (calcPos n)))
                           putChar ' '
                     else
                       return ()

                     if isEmpty (calcPos n) ps then
                         putChar '-'
                     else
                         putChar (pieceChar (head (findPiece (calcPos n) ps)))

                     if (getColumn (calcPos n) == 7) then
                         putChar '\n'
                     else
                         putChar ' '

                     printBoard (n+1) ps

printCols :: IO ()
printCols = do putStr "  0 1 2 3 4 5 6 7\n"

printCapturedPieces :: Colour -> AllPieces -> IO ()
printCapturedPieces White ps = do printPieceList (getCapturedPieces White ps)
                                  putStr " ================\n"
printCapturedPieces Black ps = do putStr " ================\n"
                                  printPieceList (getCapturedPieces Black ps)

printPieceList :: [Piece] -> IO ()
printPieceList [] = return ()
printPieceList (x:[]) = do putChar (pieceChar x)
                           putChar ' '
                           putChar '\n'
printPieceList (x:xs) = do putChar (pieceChar x)
                           putChar ' '
                           printPieceList xs

getCapturedPieces :: Colour -> AllPieces -> [Piece]
getCapturedPieces c ps = [x | x <- ps, getColour x == c, getPos x == (-1,-1)]

{-
strBoard :: Int -> AllPieces -> String -> String
strBoard 64 ps s = s
strBoard n ps s = if isEmpty (calcPos n) ps then
                      strBoard (n+1) ps (s ++ '-' : e : [])
                  else
                      strBoard (n+1) ps (s ++ show (pieceChar (head (findPiece (calcPos n) ps ))) : e : [])
                  where
                      e = if getColumn (calcPos n) == 7 then '\n' else ' '
--}
