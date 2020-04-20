module Main where

import Util
import Init
import TypeDefs
import System.IO
import Data.List

main = do
       content <- readFile "moveList.pgn"
       piece <- getLine
       color <- getLine
       row <- getLine
       column <- getLine
       moveRow <- getLine
       moveColumn <- getLine
       makeProperMove (createPiece piece color row column) (position moveRow moveColumn) (contains "King,Black" content) (contains "King,White" content) (addKings ++ addRooks)



-- returns whether a string is inside a another string
contains :: String -> String -> Bool
contains a b = isInfixOf a b

-- returns a piece from 4 strings
createPiece :: String -> String -> String -> String -> Piece
createPiece a b c d = (pieceType a, colorType b, position c d)

-- if you can think of nicer ways to do this function then let me know.
pieceType :: String -> PieceType
pieceType "Pawn" = Pawn
pieceType "Knight" = Knight
pieceType "Bishop" = Bishop
pieceType "Rook" = Rook
pieceType "Queen" = Queen
pieceType "King" = King

colorType :: String -> Colour
colorType "Black" = Black
colorType "White" = White

position :: String -> String -> (Int,Int)
position a b = (read a, read b)
