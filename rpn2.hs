{-
    The RPN calculator

    To run the program, do

    runhaskell.exe rpn2.hs "10 2 3 + -"

    You can change the string "10 2 3 + -" to others to do a different
    calculation.

    With error handling capability.
-}
import Data.List (words)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    (expression : _) <- getArgs
    print $ solveRPN expression


{-
    The problem with solveRPN is that the calculation can fail:

    1. Read string to number.
    2. Divide by zero.
    3. Log of negative value.
-}
-- solveRPN :: (Floating a, Read a) => String -> a
-- solveRPN = head . foldl foldingFunction [] . words where
--     foldingFunction (x:y:ys) "+" = (x + y) : ys
--     foldingFunction (x:y:ys) "-" = (y - x) : ys
--     foldingFunction (x:y:ys) "*" = (x * y) : ys
--     foldingFunction (x:y:ys) "/" = (y / x) : ys
--     foldingFunction (x:y:ys) "^" = (y ** x) : ys
--     foldingFunction (x:xs) "ln" = log x : xs
--     foldingFunction xs "sum" = [ sum xs ]
--     foldingFunction xs numberString = read numberString : xs


solveRPN :: String -> Maybe Float
solveRPN = head . foldl foldingFunction [] . words where
    foldingFunction (x:y:ys) "+" = (x + y) : ys
    foldingFunction (x:y:ys) "-" = (y - x) : ys
    foldingFunction (x:y:ys) "*" = (x * y) : ys
    foldingFunction (x:y:ys) "/" = (y / x) : ys
    foldingFunction (x:y:ys) "^" = (y ** x) : ys
    foldingFunction (x:xs) "ln" = log x : xs
    foldingFunction xs "sum" = [ sum xs ]
    foldingFunction xs numberString = readMaybe numberString : xs
