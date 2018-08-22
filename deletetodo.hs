{-
    file IO

    Read a text file, delete a line from it and save the change.

    The steps are:

    1. Read the todo.txt file,
    2. Show the contents and let user choose one item to remove.
    3. Save the contents except the removed one into a temporary file.
    4. Delete and todo.txt and rename the temporary file as todo.txt.
-}
import System.IO
import Data.List (delete)
import System.Directory (removeFile, renameFile)

main = do
    handle <- openFile "todo.txt" ReadMode

    -- open a temporary file in local directory
    (tempName, tempHandle) <- openTempFile "." "temp"

    -- read and show todo.txt
    contents <- hGetContents handle
    let
        todos = lines contents
        numberedTodos = zipWith (\lineNo line -> show lineNo ++ " - " ++ line)
                        [1..] todos
    putStrLn "Your todo items: "
    putStr $ unlines numberedTodos

    -- ask user on which item to delete
    putStrLn "Which item do you want to delete?"
    numberString <- getLine
    let
        number = read numberString :: Int
        newTodos = delete (todos !! (number - 1)) todos

    -- save contents into temp file
    hPutStr tempHandle $ unlines newTodos
    hClose handle
    hClose tempHandle

    -- remove todo.txt and rename temp file as todo.txt
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
