import System.Environment
import System.Exit


main = do x <- getArgs
          print (mult (head (x)) (head (tail (x))))

mult :: String -> String -> String
mult a b = a ++ b

parse ["-v"] = version >> exit
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
