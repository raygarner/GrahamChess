--module Main where

import           Data.List
--import           Data.List.Split
import           Init
import           System.IO
import           TypeDefs
import           Util
import           Data.Char
import           Search
import           Debug
import           UI
import           Data.List
import           Data.Maybe
import           Data.Time.Clock

main :: IO ()
main = gameLoop addAllPieces



gameLoop :: AllPieces -> IO ()
gameLoop ps = do printBoard (-1) ps
                 if (not (isEitherCheckmate ps)) then
                   do
                     putStr "Your turn: \n"
                     m <- getLine
                     n <- getLine
                     r <- getLine
                     c <- getLine
                     piece <- return (findPiece (convertToPos (m,n)) ps)
                     if (not (null piece)) then
                       do
                         print (head piece)
                         move <- return (buildMove (getPos (head piece)) (r,c))
                         print move
                         if (isMoveOk (head piece) move ps) then
                           do
                             ps <- return (executeMove (head piece) move ps)
                             print ps
                             printBoard (-1) ps
                             if (not (isEitherCheckmate ps)) then
                               do
                                 putStr "Graham is thinking of a move...\n"
                                 start <- getCurrentTime
                                 response <- return (findRealBestMove Black ps)
                                 if (response == ((King, White, (7,4), 0), (0,0), 0)) then
                                   do putStr ("Stalemate.\n")
                                 else
                                   do
                                   print response
                                   putStr "Graham has made his move...\n"
                                   move <- return (extractMove response)
                                   print move
                                   piece <- return (extractPiece response)
                                   print piece
                                   end <- getCurrentTime
                                   putStrLn $ show (end `diffUTCTime` start)
                                   gameLoop (executeMove piece move ps)
                             else
                               do
                                 putStr ("Checkmate.\n")
                         else
                           do
                             putStr ("Move is invalid - either your king is in check or your piece cannot move there.\n")
                             gameLoop ps
                     else
                       do
                         putStr ("A piece does not exist at that position.\n")
                         gameLoop ps
                 else
                   do
                     putStr ("Checkmate.\n")

convertToPos :: (String,String) -> (Int,Int)
convertToPos (m,n) = (8 - (read n) , fromMaybe (-1) (elemIndex m columns))

columns :: [String]
columns = ["a", "b","c","d","e","f","g","h"]

buildMove :: (Int,Int) -> (String, String) -> (Int,Int)
buildMove (m,n) (r,c) = moveMade (m,n) (convertToPos (r,c))

isMoveOk :: Piece -> Move -> AllPieces -> Bool
isMoveOk p m ps = isValidMove p m ps && not (willKingBeInCheck p m ps)
