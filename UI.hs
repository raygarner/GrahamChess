module UI where

import TypeDefs
import Init
import Util
import Data.Char

calcPos :: Int -> Pos
calcPos a = (a `div` 8, a `mod` 8)


pieceChar :: Piece -> Char
pieceChar (Pawn,c,_,_) = colourCase c 'p'
pieceChar (Knight,c,_,_) = colourCase c 'n'
pieceChar (Bishop,c,_,_) = colourCase c 'b'
pieceChar (Rook,c,_,_) = colourCase c 'r'
pieceChar (Queen,c,_,_) = colourCase c 'q'
pieceChar (King,c,_,_) = colourCase c 'k'

-- white pieces shown in upper case, black pieces in lower case
colourCase :: Colour -> Char -> Char
colourCase c s | c == White = toUpper s
               | otherwise  = toLower s

printBoard :: Int -> AllPieces -> IO ()
printBoard 64 ps = do printCapturedPieces White ps
                      printCapturedPieces Black ps
printBoard n ps = do if isEmpty (calcPos n) ps then
                         putChar '-'
                     else
                         do putChar (pieceChar (head (findPiece (calcPos n) ps)))

                     if (getColumn (calcPos n) == 7) then
                         putChar '\n'
                     else
                        do putChar ' '

                     printBoard (n+1) ps

printCapturedPieces :: Colour -> AllPieces -> IO ()
printCapturedPieces c ps = printPieceList (getCapturedPieces c ps)

printPieceList :: [Piece] -> IO ()
printPieceList [] = return ()
printPieceList (x:[]) = do putChar (pieceChar x)
                           putChar '\n'
printPieceList (x:xs) = do putChar (pieceChar x)
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

