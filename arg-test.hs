{-
    Command line arguments

    Compile the program: ghc --make arg-test.hs

    Then run:

    ./arg-test.exe first second w00t "multi word arg"
-}
import Data.Traversable (mapM)
import System.Environment (getArgs, getProgName)

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
