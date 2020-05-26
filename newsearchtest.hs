import TypeDefs
import Init
import System.IO
import UI
import OpeningSearch

main :: IO ()
main = do printBoard (-1) addAllPieces
          putStr "Graham is thinking of a move...\n"
          --tree <- return (buildBranches 0 addAllPieces White)

          --putStr (show tree)

          {--
          m <- getLine
          tree <- return (buildTree 1 Black tree)
          putStr (show tree)
          m <- getLine
          tree <- return (addLeafEval Black tree)
          putStr (show tree)
          m <- getLine
          tree <- return (propagateEval 3 White tree)

          m <- getLine
          bestboard <- return (findBestFirstBoard White tree)
          printBoard (-1) bestboard
          --}

          --printBoard (-1) (findBestFirstBoard White (propagateEval 3 White (addLeafEval Black (buildTree 1 Black (buildBranches 0 addAllPieces White)))))
          bestboard <- return (newsearchtestfunc)
          printBoard (-1) bestboard

