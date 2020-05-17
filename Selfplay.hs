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
import Data.Time.Clock

main :: IO ()
main = gameLoop White addAllPieces



gameLoop :: Colour -> AllPieces -> IO ()
gameLoop c ps = do putStr "Graham is thinking of a move...\n"
                   printBoard (-1) ps
                   start <- getCurrentTime
                   response <- return (findRealBestMove c ps)
                   print response
                   --putStr "Graham has made his move...\n"
                   move <- return (extractMove response)
                   --print move
                   piece <- return (extractPiece response)
                   --print piece
                   end <- getCurrentTime
                   putStrLn $ show (end `diffUTCTime` start)

                   gameLoop (invertColour c) (movePiece piece move ps)


buildMove :: (String, String) -> (Int,Int)
buildMove (r,c) = (read r, read c)
