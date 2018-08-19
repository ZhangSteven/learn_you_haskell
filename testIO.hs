{-
    Input and Output
-}
import Data.Char (toUpper)
import Control.Monad (when, forever, mapM, forM)

{-
    Hello, World

    Function signature for putStrLn,

    putStrLn :: String -> IO ()

    perform an IO action and return value is ()
}
    main = putStrLn "Hello, World!"
-}

{-
    The below program takes a line of input and then print it out.

    getLine :: IO String

    perform an IO action and return value is String

    <- operator: take the string from IO action and bind it to
    a name.

    However, when the program starts, it is pending for input without
    showing the "Hello, ..." line. Only after we type and press Enter,
    it shows the "Hello, ..." line and the "Hey ..." line.
-
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!"
--}


{-
    Use "Let" in IO actions.

    NOTE:
    1. There is no "in" in the let.
    2. Same as above, only after we input firstName and lastName
    the "What's your ..." are displayed on the screen.
-
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let
        bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName
                ++ ", how are you?"
-}


{-
    Take a multi line input, until a blank line is hit.

    NOTE: the return statement does not terminate the program like other
    imperative languages. Instead, it takes a value and creats an I/O action
    without doing anything, the value wrapped in that I/O action is just the
    value itself. We use return () simply because the two branches of
    if .. else .. need to evaluate to the same type of value, which is IO ().

    putStrLn :: IO ()   -- IO action with return value ()

    return () :: IO ()  -- IO action (that does nothing) with return value ()

    if we do:

    value <- return ()

    we will get value = ().
-
main = do
    line <- getLine
    if null line then   -- test for blank line
        return ()
    else do
        putStrLn $ reverseWords line
        main


reverseWords :: String -> String
reverseWords =
    unwords . map reverse . words
-}



{-
    Test return again.

    return is an IO action that does nothing. return xxx returns
    xxx value. It doesn't change the program control flow at all.

    NOTE: do block's return value is the return value of the last
    IO statement, in this case purStrLn.

    The print function is actually putStrLn . show, i.e., convert
    something to string using show, the call purStrLn to do the
    IO action.

    The difference between putStrLn and print is that, for a
    string,

    putStrLn "hello" -- hello
    print "hello"    -- "hello" (there is a double quote)
-
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return 5
    -- putStrLn line
    print line
-}


{-
    Echo everything you type.

    But it doesn't start echoing as you type, only when you
    hit Enter.
-
main = do
    c <- getChar
    if c /= ' ' then do
        putChar c
        main
    else
        return ()

    The above code is the same as using the when function:
-

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
-}


{-
    Asking for input and echo it, repeat this action forever.


main = forever $ do
    putStr "Give me some input"
    line <- getLine
    putStrLn $ map toUpper line
-}


{-
    sequence: take a list of IO actions, execute them in turn.

    e.g.,

    rs <- sequence [getLine, getLine, getLine]

    get 3 lines of input and assign them into list rs.

    e.g.,

    sequence $ map print [1..3]
    1
    2
    3
    [(), (), ()]

    NOTE: simply doing map print [1,2,3] won't print out anything, because
    that will just give [print 1, print 2, print 3] without executing them.

    Since mapping a function that returns an I/O action over a list and then
    sequencing it is so common, the utility functions mapM and mapM_ are
    introduced. E.g,

    mapM print [1,2,3]
    1                   -- side effect of the print function
    2
    3
    [(), (), ()]        -- return value after the mapping

    The only difference between mapM and mapM_ is that mapM_ discards the
    return value.

    forM is very similar to mapM, only with the postion of the input flipped.
    Here is an example,

    NOTE: the mapping function we supply with mapM, forM must return an I/O
    action, e.g., withing a "do" block.
-}
main = do
    colors <- forM [1, 2, 3, 4] (\a -> do
        putStrLn $ "Which color do you associate with color " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
