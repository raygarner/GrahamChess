--module Selfplay where

import           Data.List
--import           Data.List.Split
import           Init
import           System.IO
import           TypeDefs
import           Util
import           Data.Char
import           Search
import           UI
import           Debug
import           Eval

main :: IO ()
main = gameLoop addAllPieces



gameLoop :: AllPieces -> IO ()
gameLoop ps = do putStr "\n"
                 printBoard (-1) ps
                 putStr "Your turn:\n"
                 m <- getLine
                 n <- getLine
                 r <- getLine
                 c <- getLine
                 piece <- return (head (findPiece (read m, read n) ps))
                 move <- return (buildMove (r,c))
                 gameLoop (executeMove piece move ps)


buildMove :: (String, String) -> (Int,Int)
buildMove (r,c) = (read r, read c)
