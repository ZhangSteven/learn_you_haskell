{-

Create a program to view, add and delete todos.

To add a task:
todo add todo.txt "Find the magic sword of power"

To view all tasks:
todo view todo.txt

To delete a task:
todo remove todo.txt 2

-}
import System.Environment (getArgs)
import System.Directory (renameFile, removeFile)
import System.IO (appendFile, withFile, IOMode (ReadMode), hGetContents,
                    hPutStr, openTempFile, openFile, hClose)
import Data.List (lookup, delete)


dispatch :: [ (String, [ String ] -> IO ()) ]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           ]


main :: IO ()
main = do
    {- pattern matching, first argument goes to command, and the rest
    goes to args. First argument should be one of "add", "view" or
    "remove". -}
    (command : args) <- getArgs
    case (lookup command dispatch) of
        Just action -> action args
        Nothing -> return ()


-- add one line to todo file
add :: [String] -> IO ()
add [fileName, todoItem] =
    appendFile fileName (todoItem ++ "\n")


-- view contents in the file
view :: [String] -> IO ()
view [fileName] =
    withFile fileName ReadMode (\handle -> do
        contents <- hGetContents handle
        let
            todos = lines contents
            numberedTodos = zipWith (\a b -> show a ++ " - " ++ b) [1..] todos
        putStr $ unlines numberedTodos )


-- delete a line in todo file
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let
        todos = lines contents
        lineNo = read numberString
        newTodos = delete (todos !! (lineNo - 1)) todos
    hPutStr tempHandle $ unlines newTodos
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
