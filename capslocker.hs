{-
    Test I/O function


-}
import Data.Char (toUpper)
import Control.Monad (forever)


{-

Compile this program into capslocker.exe, then do (in GitBash):

cat haiku.txt | ./capslocker.exe

This way, we can pipe the output of one program (cat) to the input of
another (capslocker.exe) via the "|" character.

main = forever $ do
    putStr "Give me some input: "
    line <- getLine
    putStrLn $ map toUpper line

The forever stops when the end of file (EOF) character is read.

Or, we can do the below to realize similar effect, with IO action
getContents. Here we bind the string read by getContents to contents.

NOTE: getContents is I/O lazy, it won't try to read everything at once and
store it into memeory before printing out the capslocked version. Rather,
it will print out the capslocked version as it reads it, because it will only
read a line from the input when it really needs to.

-}
main = do
    contents <- getContents
    putStr (map toUpper contents)
    -- putStrLn (map toUpper contents)
