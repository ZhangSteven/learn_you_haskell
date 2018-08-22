{-
    Handle exceptions.

    Haskell code can throw exceptions. Like "head []" or read a file that
    does not exist. However, we can only catch the exceptions in I/O part
    of our code.

    Try play around with it:

    -- No file provided
    runhaskell.exe testException.hs

    -- nonExistent file
    runhaskell.exe testException.hs file_not_exist.txt
-}

import System.Environment (getArgs)
import System.IO (readFile)
import System.IO.Error (IOError, isDoesNotExistError, ioeGetFileName)
import Control.Exception (catch)

main :: IO ()
main =
    toTry `catch` handler


toTry :: IO ()
toTry = do
    (fileName : _) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length $ lines contents) ++ " lines."


handler :: IOError -> IO ()
handler e
    -- This works, but we can use a case statement as well
    -- | isDoesNotExistError e = putStrLn "The file does not exist."
    | isDoesNotExistError e =
        case ioeGetFileName e of
            Just path -> putStrLn $ "Ooops! File does not exist: " ++ path
            Nothing -> putStrLn "Ooops! File does not exist at unknown location"
    | otherwise = ioError e
