{-
    Test I/O function
-}


{-

After compile the program, do (in GitBash):

cat shortlines.txt | ./shortlinesonly.exe

For another example on I/O action getContents, see capslocker.hs

The below code works.

main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

However, since the pattern of getting some string from the input, transforming
it with a function and then outputting that is so common that there exists a
function, interact to handle.

interact :: (String -> String) -> IO ()

let's modify the program to use interact.

-}

main = interact shortLinesOnly

shortLinesOnly :: String -> String
{-

The below code works.

shortLinesOnly input =
    let
        allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
    in
        unlines shortLines

But since the steps are: lines -> filter -> unlines, we can rewrite it as
below:

-}
shortLinesOnly =
    unlines . filter ((<10) . length) . lines
