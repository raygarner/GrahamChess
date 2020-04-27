module Main where

import           Data.List
--import           Data.List.Split
import           Init
import           System.IO
import           TypeDefs
import           Util
import           Data.Char
import           Search

main :: IO ()
main = gameLoop addAllPieces



gameLoop :: AllPieces -> IO ()
gameLoop ps = do putStr "Your turn: \n"
                 m <- getLine
                 n <- getLine
                 r <- getLine
                 c <- getLine
                 piece <- return ( head (findPiece (read m, read n) ps))
                 print piece
                 move <- return (buildMove (r,c))
                 print move
                 ps <- return (movePiece piece move ps)
                 print ps
                 putStr "Graham is thinking of a move...\n"
                 response <- return (findRealBestMove Black ps)
                 print response
                 putStr "Graham has made his move...\n"
                 move <- return (extractMove response)
                 print move
                 piece <- return (extractPiece response)
                 print piece
                 gameLoop (movePiece piece move ps)


buildMove :: (String, String) -> (Int,Int)
buildMove (r,c) = (read r, read c)
