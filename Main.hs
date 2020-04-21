module Main where

import           Data.List
import           Data.List.Split
import           Init
import           System.IO
import           TypeDefs
import           Util

{-|
main = do
       content <- readFile "movelist.pgn"
       piece <- getLine
       color <- getLine
       row <- getLine
       column <- getLine
       moveRow <- getLine
       moveColumn <- getLine
       makeProperMove (createPiece piece color row column) (position moveRow moveColumn) (contains "King,Black" content) (contains "King,White" content) (addKings ++ addRooks)
-}

--main = do content <- readFile "movelist.pgn"
main :: IO AllPieces
main = do inh <- openFile "movelist.pgn" ReadMode
          content <- readFile "movelist.pgn"
          line <- hGetLine inh
          let info = splitOn ";" line
          let piece = buildPiece info
          let move = buildMove info
          makeProperMove piece move (contains "King;Black" content) (contains "King;White" content) (addKings ++ addRooks)

-- returns whether a string is inside a another string
contains :: String -> String -> Bool
contains a b = isInfixOf a b

-- returns a piece from 4 strings
createPiece :: String -> String -> String -> String -> Piece
--createPiece a b c d = (pieceType a, colorType b, position c d)
createPiece a b c d = (read a, read b, (read c, read d))


-- builds a piece from a list of info
buildPiece :: [String] -> Piece
buildPiece (piece:colour:row:column:_) = (read piece, read colour, (read row, read column))
buildPiece _ = (Rook,Black,(0,0)) --never ran, compiler just wanted a default

-- builds a move from a list of info
buildMove :: [String] -> Move
buildMove (_:_:_:_:m:n) = (read m, read (head n))
buildMove _             = (0,0) --never ran, compiler just wanted a default

-- if you can think of nicer ways to do this function then let me know.
    -- i think the 'read' function can be used instead? - ray
pieceType :: String -> PieceType
pieceType "Pawn"   = Pawn
pieceType "Knight" = Knight
pieceType "Bishop" = Bishop
pieceType "Rook"   = Rook
pieceType "Queen"  = Queen
pieceType "King"   = King

colorType :: String -> Colour
colorType "Black" = Black
colorType "White" = White

position :: String -> String -> (Int,Int)
position a b = (read a, read b)
