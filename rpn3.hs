{-
    The RPN calculator

    To run the program, do

    runhaskell.exe rpn2.hs "10 2 3 + -"

    Try the following input:

    runhaskell.exe rpn2.hs "20 10 10 - / 6 +"
    runhaskell.exe rpn2.hs "10 2 + 50 6 - ln"
    runhaskell.exe rpn2.hs "2c 10 2 + 5 6 -"
    runhaskell.exe rpn2.hs "10 2 + 5 6 - ln"
    runhaskell.exe rpn2.hs "5 + 2 5 -"

    This version's error handling is different from version2, here we
    use >>= function.
-}
import Data.List (words)
import System.Environment (getArgs)
import Text.Read (readMaybe)


main :: IO ()
main = do
    (expression : _) <- getArgs
    putStrLn $ maybe "Error" show $ getResult $ solveRPN $ words expression

evalDiv :: Maybe Float -> Maybe Float -> Maybe Float
evalDiv x y =
    do
        x' <- x
        y' <- y
        safeDiv x' y'


safeLn :: Float -> Maybe Float
safeLn x =
    if x > 0 then
        Just (log x)
    else
        Nothing


safeDiv :: Float -> Float -> Maybe Float
safeDiv x y =
    if y == 0 then
        Nothing
    else
        Just (x / y)


solveRPN :: [String] -> [Maybe Float]
solveRPN = foldl foldingFunction [] where
    foldingFunction :: [Maybe Float] -> String -> [Maybe Float]
    foldingFunction (x : y : ys) "+" = ((+) <$> y <*> x) : ys
    foldingFunction (x : y : ys) "-" = ((-) <$> y <*> x) : ys
    foldingFunction (x : y : ys) "*" = ((*) <$> y <*> x) : ys
    foldingFunction (x : y : ys) "/" = evalDiv y x : ys
    foldingFunction (x : xs) "ln" = (x >>= safeLn) : xs
    foldingFunction xs x = (readMaybe x :: Maybe Float) : xs


{-
    When we got input like "2c 5 8 +" or "+ + + 6", that will lead to
    invalid result (Nothing) into the list returned by solveRPN. So,

    1. return the head of the list, if none of the elements in the list
        is Nothing.
    2. Othrewise return Nothing.

    Here we use sequenceA to solve this problem neatly,

    sequenceA :: (Applicative t) => [t a] -> t [a]

    In the case of Maybe, if anyone in the list is Nothing, then the result
    is Nothing, otherewise it is Just [...]

    sequenceA [Just 5, Just 6, Just 7] -> Just [5, 6, 7]
    sequenceA [Just 5, Nothing, Just 8] -> Nothing
-}
getResult :: [Maybe Float] -> Maybe Float
getResult list =
    fmap head $ sequenceA list
