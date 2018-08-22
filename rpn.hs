{-
    The RPN calculator
-}
import Data.List (words)
import System.Environment (getArgs)

main :: IO ()
main = do
    (expression : _) <- getArgs
    print $ solveRPN expression

-- This signature works before adding "/" to the operators.
-- solveRPN :: (Num a, Read a) => String -> a
solveRPN :: (Floating a, Read a) => String -> a
{-
solveRPN expression =
    head (foldl foldingFunction [] (words expression)) where
        foldingFunction stack item = ...

    There is a better way to use function composition as below:
-}

solveRPN = head . foldl foldingFunction [] . words where
    foldingFunction (x:y:ys) "+" = (x + y) : ys
    foldingFunction (x:y:ys) "-" = (y - x) : ys
    foldingFunction (x:y:ys) "*" = (x * y) : ys
    foldingFunction (x:y:ys) "/" = (y / x) : ys
    foldingFunction (x:y:ys) "^" = (y ** x) : ys
    foldingFunction (x:xs) "ln" = log x : xs
    foldingFunction xs "sum" = [ sum xs ]
    foldingFunction xs numberString = read numberString : xs
