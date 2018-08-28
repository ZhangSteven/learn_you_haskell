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

    Checkout version 3, rpn3.hs, which I think is the best solution.
-}
import Data.List (words)
import System.Environment (getArgs)
import Text.Read (readMaybe)


main :: IO ()
main = do
    (expression : _) <- getArgs
    putStrLn $ maybe "Error" show $ showResult $ solveRPN
        $ map toOpItem $ words expression


data OpItem = Val Float
            | ValNan
            | Add
            | Sub
            | Mul
            | Div
            | Ln
            deriving Show


toOpItem :: String -> OpItem
toOpItem "+" = Add
toOpItem "-" = Sub
toOpItem "*" = Mul
toOpItem "/" = Div
toOpItem "ln" = Ln
toOpItem s =
    case (readMaybe s :: Maybe Float) of
        Just x -> Val x
        Nothing -> ValNan


safeLn :: Float -> OpItem
safeLn x =
    if x > 0 then
        Val (log x)
    else
        ValNan


safeDiv :: Float -> Float -> OpItem
safeDiv x y =
    if y == 0 then
        ValNan
    else
        Val (x / y)


solveRPN :: [OpItem] -> [OpItem]
solveRPN = foldl foldingFunction [] where
    foldingFunction (Val x : Val y : ys) Add = Val (x + y) : ys
    foldingFunction (Val x : Val y : ys) Sub = Val (y - x) : ys
    foldingFunction (Val x : Val y : ys) Mul = Val (x * y) : ys
    foldingFunction (Val x : Val y : ys) Div = safeDiv y x : ys
    foldingFunction (Val x : xs) Ln = safeLn x : xs
    foldingFunction xs op = op:xs


{-
    When we got input like "2c 5 8 +" or "+ + + 6", the non-number
    values and extra operators (+/-/*...) will remain in the list
    from solveRPN. So we need to verify everything left in the list
    is numbers (isValue). Othrewise the result is invalid.
-}
showResult :: [OpItem] -> Maybe Float
showResult list =
    if not (null list) && all isValue list then
        case head list of
            Val x -> Just x
            _ -> Nothing
    else
        Nothing
    where
        isValue :: OpItem -> Bool
        isValue op =
            case op of
                Val _ -> True
                _ -> False
