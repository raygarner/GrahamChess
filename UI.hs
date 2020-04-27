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
printBoard 64 ps = return ()
printBoard n ps = do if null (findPiece (calcPos n) ps) then
                         putChar '-'
                     else
                         do putChar (pieceChar (head (findPiece (calcPos n) ps)))

                     if (getColumn (calcPos n) == 7) then
                         putChar '\n'
                     else
                        do putChar ' '

                     printBoard (n+1) ps


