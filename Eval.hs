module Eval where

import           TypeDefs
import           Util
import           Debug
import           EvalOpening
import           EvalMiddle
import           EvalEnd
import           Debug.Trace

totalVal :: Colour -> AllPieces -> Float
totalVal c ps | getGamePoint ps == Opening = totalOpeningVal ps
              | getGamePoint ps == Middle = totalOpeningVal ps --totalMiddleVal c ps (temporary to make testing easier)
              | otherwise = totalEndVal ps

--totalValUnsafe :: Colour -> AllPieces -> Float
--totalValUnsafe c ps = totalOpeningValUnsafe c ps
