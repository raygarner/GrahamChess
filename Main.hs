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

main :: IO ()
main = gameLoop addAllPieces



gameLoop :: AllPieces -> IO ()
gameLoop ps = do printBoard 0 ps
                 if (not (isEitherCheckmate ps)) then
                   do
                     putStr "Your turn: \n"
                     m <- getLine
                     n <- getLine
                     r <- getLine
                     c <- getLine
                     piece <- return (findPiece (read m, read n) ps)
                     if (not (null piece)) then
                       do
                         print (head piece)
                         move <- return (buildMove (r,c))
                         print move
                         if (isMoveOk (head piece) move ps) then
                           do
                             ps <- return (executeMove (head piece) move ps)
                             print ps
                             printBoard 0 ps
                             if (not (isEitherCheckmate ps)) then
                               do
                                 putStr "Graham is thinking of a move...\n"
                                 response <- return (findRealBestMove Black ps)
                                 print response
                                 putStr "Graham has made his move...\n"
                                 move <- return (extractMove response)
                                 print move
                                 piece <- return (extractPiece response)
                                 print piece
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

buildMove :: (String, String) -> (Int,Int)
buildMove (r,c) = (read r, read c)

isMoveOk :: Piece -> Move -> AllPieces -> Bool
isMoveOk p m ps = isValidMove p m ps && not (willKingBeInCheck p m ps)
