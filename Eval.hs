module Eval where

import           TypeDefs
import           Util
import           Debug
import           EvalOpening
import           EvalMiddle
import           EvalEnd
import           Debug.Trace

totalVal :: Colour -> AllPieces -> Float
totalVal c ps | getGamePoint ps == Opening = totalOpeningVal c ps
              | getGamePoint ps == Middle = totalOpeningVal c ps --totalMiddleVal c ps (temporary to make testing easier)
              | otherwise = totalEndVal c ps

totalValUnsafe :: Colour -> AllPieces -> Float
totalValUnsafe c ps = totalOpeningValUnsafe c ps


