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
import Control.Monad (guard)

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
        x1 <- expression 1
        x2 <- expression 2
        ...
        xn <- expression n
        f x1 x2 ... xn

    The above is exactly the same as the nested >>= calls below:

    expression 1 >>= \x1 ->
        expression 2 >>= \x2 ->
            ...
                expression n >>= \xn ->
                    f x1 x2 ... xn

    In the case of Maybe, the value of the do block is Nothing if any of
    x1..xn is Nothing, otherwise the result of the function call.
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
    More on do block.

    In a do block, if you can put values stand alone, like below:

    do
        x1 <- expression 1
        y
        x2 <- expression 2
        f x1 x2

    Then it's equivalent to:

    expression 1 >>= \x1 ->
        y >>= \_ ->
            expression 2 >>= \x2 ->
                f x1 x2

    Since y1 has no effect in the call f x1 x2, it's like a fail or succeed
    swith. See below example:
-}
resultDo = do
    x1 <- safeDiv 20 10
    Nothing
    x2 <- safeLn x1
    return x2       -- Nothing


{-
    The >> function

    x >> y = x >>= \_ -> y

    In the case of Maybe,
    Nothing >> y = Nothing
    x >> y       = y

    it means if x is a failure (Nothing), then the failure propogates,
    otherwise take the value y.

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

    In the below example, when we give an empty string as input, the pattern
    matching fails, then fail will be called and yields Nothing in the do
    block. Then return x won't be called at all, since the do block evalutes
    to Nothing directly.
-}
firstChar :: [Char] -> Maybe Char
firstChar msg = do
    (x : _) <- Just msg
    return x

result5 = firstChar "Hello" -- Just 'H'
result6 = firstChar ""      -- Nothing


{-
    The List Monad

    instance Monad [] where
        return x = [x]

        (>>=) :: [x] -> (x -> [y]) -> [y]
        xs >>= f =
            concat $ map f xs

        fail _ = []

    Here is how it works.
-}
numberToString :: (Num a, Ord a) => a -> String
numberToString x
    | x > 0 = "Pos "
    | otherwise = "Neg "

resultL1 = [1, -2, 0, 5.5] >>= numberToString   -- "Pos Neg Neg Pos "


{-
    See what happens on chain >>= calls on [x]

    NOTE: the "return (n, ch)"" wraps the tuple in the list context, i.e.,
    it returns [(n, ch)], so that the function confirms to type x -> [y]

    result : [(1,'a'), (1,'b'), (2,'a'), (2,'b')]
-}
resultL2 = [1, 2] >>= \n ->
                ['a', 'b'] >>= \ch ->
                    return (n, ch)

-- Let's translate it to the do notation
resultL3 = do
    n  <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)

-- Let's do it again in list comprehension, it's the same. In fact, list
-- comprehensios are just syntactic sugar for using lists as Monads.
resultL4 = [ (n, ch) | n <- [1, 2], ch <- ['a', 'b'] ]


{-
    Monads that can act as Monoids : MonadPlus class

    class (Monad m) => MonadPlus where
        mzero :: m a
        mplus :: m a -> m a -> m a

    Compared to Monoid class definition:

    class Monoid m where
        mempty :: m
        mappend :: m -> m -> m
        mconcat :: [m] -> m
        mconcat =
            foldr mconcat mempty

    Here mzero and mplus are equivalents of mempty and mappend.

    Since lists are monoids as well as monads, they can be made an instance
    of this type class:

    instance MonadPlus [] where
        mzero = []
        mplus = (++)

    For MonadPlus instances, there is a guard function:

    guard :: (MonadPlus m) => Bool -> m ()
    guard True = return ()
    guard False = mzero

    (from the Hasekell documentation, guard seems to use Applicative m,
    which is a weaker version of MonadPlus, let's use the above first)

    Let's see how it works on list comprehension with filters.
-}
sevens = [ x | x <- [1..50], '7' `elem` (show x) ]  -- [7,17,27,37,47]

-- () is both a type (tuple with zero elements) and the only value
-- in that type. Here Maybe () and [()] are type definitions.
resultG1 = guard (1 > 2) :: [()]        -- []
resultG2 = guard (1 > 2) :: Maybe ()    -- Nothing
resultG3 = guard (1 < 2) :: [()]        -- [()]
resultG4 = guard (1 < 2) :: Maybe ()    -- Just ()


-- use guard in the do notation
sevens2 = do
    x <- [1..50]
    guard $ '7' `elem` (show x)
    return x


{-
    How guard works: translate do notation to chained >>= calls

    When guard False happens, it returns mzero (mempty), according to
    MonadPlus laws,

    mzero >>= f = zero

    In the case of below example, say x = 8, guard False evaluates to [],
    then the expression becomes

    \8 ->
        [] >>= f       -- evaluates to [] regardless of f

    then the result is [].

    If x = 7, then the expression becomes

    \7 ->
        [()] >>= \_ -> return 7

    It's the same as map (\_ -> return 7) [()], which is just [7].

    Therefore, guard is like a switch in the computation, if it is False,
    the computation will always yield mzero regardless of the following
    code due to the MonadPlus law (mzero >>= f = zero). If it is True,
    it creates a dummy value that simply gets ignored in the following
    computation. That's how guard works like a fiter.
-}
sevens3 =
    [1..50] >>= \x ->
        guard ('7' `elem` (show x)) >>= \_ ->
            return x


{-
    Monad Laws

    Just like applicative functors, and all functors before them, monads
    come with a few laws that all monad instances must abide by. Just because
    something is made an instance of Monad type class doesn't mean that it's
    a monad.

    For a type to truly be a monad, the monad laws must hold for that type.
    These laws allow us to make reasonable assumptions about the type and its
    behaviour.

    1. Left Identity.

    return x >>= f is the same as f x

    Let's try, for [],

    return x >>= f =
        concat $ map f [x]      -- it's f x, f :: a -> [b]

    for Maybe a,

    return x >>= f =
        Just x >>= f            -- it's f x, f :: a -> Maybe b

    2. Right Identity.

    m >>= return is the same as m

    Let's try, for the case of [a],
    [] >>= return                   -- it's []
    [x1, x2..] >>= return =
        concat $ map return [..]    -- concat [[x1], [x2]..] = [x1, x2..]

    for the case of Maybe a,
    Nothing >>= return              -- it's Nothing
    Just x  >>= return              -- Just x

    3. Associativity.

    (m >>= f) >>= g is the sames as m >>= \x -> f x >>= g

    For [a] and Maybe a, their mempty value [] and Nothing satisfy the
    following:

    mempty >>= f = mempty, therefore the rule holds.

    For [x1, x2..],

    ([x1, x2..] >>= f) >>= g =
        concat [f x1, f x2..] >> g =
        [y11..y1k, y21..y2k..] >> g =
        concat [g y11..g y1k, g y21..g y2k, ..]

    [x1, x2..] >>= \x -> f x >>= g =
        concat [f x1 >> g, f x2 >> g ..] =
        concat [g y11..g y1k, g y21..g y2k, ..]

    For Just x,

    (Just x >> f) >>= g =
        f x >> g

    Just x >>= \x -> f x >> g =
        f x >> g

    Therefore, for [a] and Maybe a, Associativity holds.
-}
