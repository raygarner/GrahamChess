module Selfplay where

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

main :: IO ()
main = gameLoop White addAllPieces



gameLoop :: Colour -> AllPieces -> IO ()
gameLoop c ps = do putStr "Graham is thinking of a move...\n"
                   printBoard 0 ps
                   response <- return (findRealBestMove c ps)
                   print response
                   --putStr "Graham has made his move...\n"
                   move <- return (extractMove response)
                   --print move
                   piece <- return (extractPiece response)
                   --print piece
                   gameLoop (invertColour c) (movePiece piece move ps)


buildMove :: (String, String) -> (Int,Int)
buildMove (r,c) = (read r, read c)
