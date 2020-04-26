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
                 let piece = head (findPiece (read m, read n) addAllPieces)
                 let move = buildMove (r,c)
                 let ps = movePiece piece move ps
                 putStr "Graham is thinking of a move...\n"
                 let response = findRealBestMove Black ps
                 let move = extractMove response
                 let piece = extractPiece response
                 gameLoop (movePiece piece move ps)


buildMove :: (String, String) -> (Int,Int)
buildMove (r,c) = (read r, read c)




