{-
    A Fistful of Monads.

    We already have Functor and Applicative type class that allows us to
    do computation in a context:

    For a Functor, we have fmap which allows us to apply a plain function
    on a value in a Functor instance.

    fmap :: (Functor t) => (a -> b) -> t a -> t b

    However, if a function is in a context, say Just (+ 3), then we cannot
    do fmap. What's more, we cannot apply a function on multiple Functor
    instances. Applicative comes to the rescue, we can do:

    pure (a -> b -> c) <*> t a <*> t b  -- t is Applicative

    Now we have Monad type class,

    class Monad m where
        return :: a -> m a  -- same as pure
        (>>=) :: m a -> (a -> m b) -> m b

        (>>) :: m a -> m b -> m b
        x >> y = x >>= \_ -> y

        fail :: String -> m a   -- called when things like pattern matching
        fail msg = error msg    -- failed. See later examples.

    >>=  used to apply function (a -> m b) on m a
    fmap used to apply function (a -> b  ) on t a
-}
import Data.Maybe (maybe)


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


{-
    Evaluate expression that can fail.
-}
eval :: Expr -> Maybe Float
eval (Val v) = Just v
eval ValNan = Nothing
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y

{-
    eval x :: Maybe Float
    safeLn :: Float -> Maybe Float

    Therefore, to evaluate (Ln x), we need the >>= function,

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-}
eval (Ln x) =
    eval x >>= safeLn

{-
    eval x :: Maybe Float
    eval y :: Maybe Float
    safeDiv :: Float -> Float -> Maybe Float

    In this case, we need nested >>= calls.

    eval x >>= \x' ->
        eval y >>= \y' ->
            safeDiv x' y'

    Or better formatted as:
    eval x >>= \x' ->
    eval y >>= \y' ->
    safeDiv x' y'

    The pattern is so common that a do notation is created to represent it:

    do
        x1 <- m x1
        x2 <- m x2
        ...
        xn <- m xn
        f x1 x2 ... xn  -- return value should be m x

    The value of the do block is Nothing if any of x1..xn is Nothing, or the
    result of the function call.
-}
eval (Div x y) = do
    x' <- eval x
    y' <- eval y
    safeDiv x' y'


-- Since log can throw exception, use Maybe Float to represent result
safeLn :: Float -> Maybe Float
safeLn x
    | x > 0 = Just (log x)
    | otherwise = Nothing


-- Since divided by zero is not acceptable, use Maybe Float to represent.
safeDiv :: Float -> Float -> Maybe Float
safeDiv x y
    | y == 0 = Nothing
    | otherwise = Just (x / y)


{-
    More on do block (Maybe)

    In a do block, you can put values stand alone. If a Nothing appears,
    then whole expression evaluates to Nothing.
-}
resultDo = do
    x1 <- safeDiv 20 10
    Nothing
    Just 100
    x2 <- safeLn x1
    return x2


{-
    The >> function

    x >> y = x >>= \_ -> y

    For the case of Maybe, if x is a failure (Nothing), then the failure
    propogates, otherwise take the value y.

    We put return 10 at the beginning of the function call because
    the input we need is of type m a, which is Just a in the case of
    Maybe.
-}
result1 = return 10 >>= safeDiv 20 >>= safeLn               -- log 2
result2 = return 10 >>= safeDiv 20 >> Just 88 >>= safeLn    -- log 88
result3 = return 10 >>= safeDiv 20 >> Nothing >>= safeLn    -- Nothing
result4 = return 0 >>= safeDiv 20 >> Just 88 >>= safeLn     -- Nothing


{-
    The fail function.

    Monads have a fail function. When pattern matching fails in the below
    function, fail gets called. For Maybe, it's implemented as:

    fail _ = Nothing

    Then Nothing will be put into the do block, therefore the result will
    be Nothing.
-}
firstChar :: [Char] -> Maybe Char
firstChar msg = do
    (x : _) <- Just msg
    return x

result5 = firstChar "Hello" -- Just 'H'
result6 = firstChar ""      -- Nothing
