{-
    Monad

    What problem does it solve?

    What good does this concept do?
-}
import Data.Maybe (maybe, fromMaybe)


{-
    Let's take a simple example.

    We have an expression that can yield some value or fail.
-}
main :: IO ()
main =
    {-
        To extract value out of Maybe a, we have a few choices∷

        1. fromJust :: Maybe a -> a, but it can throw an exception
        2. fromMaybe :: a -> Maybe a -> a, take a default value, does
            not throw exception.
        3. maybe :: b -> (a -> b) -> Maybe a -> b, apply a transformation
            (a -> b) to Maybe a when it is not Nothing and return the result,
            otherewise return the default value.

        Here we choose maybe function because we need to convert our
        data to String.
    -}
    -- works, but maybe is more compact.
    -- putStrLn $ fromMaybe "Nothing" $ fmap show $ eval expression where
    putStrLn $ maybe "Nothing" show $ eval expression where
        -- expression = Add (Val 88) (Div (Val 6) (Val 2)) -- 91
        -- expression = Add (Val 10) (Div (Val 6) (Val 0)) -- Nothing
        -- expression = Add (Val 10) (Ln (Val 0)) -- Nothing
        -- expression = Sub (Add (ValNan) (Val 10)) (Ln (Val 2.72)) -- Nothing
        expression = Sub (Add (Val 5) (Val 10)) (Ln (Val 2.72)) -- 13.999


data Expr = Val Float
            | ValNan
            | Div Expr Expr
            | Ln Expr
            | Add Expr Expr
            | Sub Expr Expr


eval :: Expr -> Maybe Float
eval (Val v) = Just v
eval ValNan = Nothing
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y

{-
    If expression x evaluated to Nothing, then result is Nothing.
    Othrewise expression evaluated to Just value, then apply value to function
    safeLn (Maybe Float).

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    mx >>= f =
        case mx of
            Nothing -> Nothing
            Just x  -> f x
-}
eval (Ln x) =
    eval x >>= safeLn

{-
    What if there are more than one undetermined input and we need:

    Maybe a -> Maybe b -> (a -> b -> Maybe c) -> Maybe c ?

    It can be done with nested >>= operators, or better, use the do notation,
    which is a short hand of the nested >>= operators.
-}
eval (Div x y) =
    do
        x' <- eval x
        y' <- eval y
        safeDiv x' y'

    -- same as below
    -- eval x >>= \x' ->
    --     eval y >>= \y' ->
    --         safeDiv x' y'

    -- The above can be better formatted as
    -- eval x >>= \x' ->
    -- eval y >>= \y' ->
    -- safeDiv x' y'


-- Since log can throw exception, use Maybe Float to represent result
safeLn :: Float -> Maybe Float
safeLn x =
    if x > 0 then
        Just (log x)
    else
        Nothing


-- Since divided by zero is not acceptable, use Maybe Float to represent.
safeDiv :: Float -> Float -> Maybe Float
safeDiv x y =
    if y == 0 then
        Nothing
    else
        Just (x / y)
