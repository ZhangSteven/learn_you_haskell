{-
    Functor and Applicative Functor.

    From:

    Learn you a haskell

    Programming in haskell (2nd Edition)
-}
import Control.Applicative


-- input two lines and see what happens
main :: IO ()
main = do
    string <- myAction  -- see below on how myAction is implemented as
                        -- Applicative
    putStrLn string


{-
    Definition of Functor typeclass.

    At times, we need to transform values in containers, like values in
    list, wrapped by Maybe, in Either, or IO. This when the Functor
    typeclass comes in handy.

    The definition is:

    class Functor t where
        fmap :: (a -> b) -> t a -> t b

    t is a parameterized type of kind "* -> *", if it supports the fmap
    function, then it belongs to typeclass Functor. For example, t can be
    Maybe, [], Either a or IO.
-}
newList = fmap (*2) [1, 2, 3]   -- [2, 4, 6]
newMaybe1 = fmap (*2) (Just 5)  -- Just 10


{-
    However, the transformation does not necessarily take the form of
    a -> b, what about a -> b -> c? For example, can we do:

    (+) (Just 5) (Just 10) = Just 15 ?

    To do so, we need to define a new version of fmap, say famp2:

    class Functor t where
        fmap :: (a -> b) -> t a -> t b
        fmap2 :: (a -> b -> c) -> t a -> t b -> t c

    then we can do:

    fmap2 (+) Just 5 Just 10

    to solve the above problem.

    But what about another function a -> b -> c -> d? For example,

    (,,) Just 5 Just 10 Just 20 = Just (5, 10, 20) ?

    Then we are going to need to define yet another version, fmap3 to solve
    this problem.

    Therefore, we may need unlimited number of versions of fmap to complete
    the definition of Functor typeclass.

    To solve this problem, two functions are defined:

    pure :: (Functor t) => a -> t a
    <*>  :: (Functor t) => t (a -> b) -> t a -> t b

    with pure, we can embed a function inside a Functor type, say, (+) to
    Just (+). Then, with <*>, we can apply that function to value embedded
    in another Functor instance. For example,

    pure (+) <*> Just 5 <*> Just 10 == Just 15

    Functors supporting pure and <*> functions are called Applicative Functor,
    a subset of Functor typeclass.

    See how it works:
-}
embeddedf = pure (+) -- pure (+) :: (Num a, Applicative f) => f (a -> a -> a)

-- Apply <*> once to become a curried function
embeddedf' = embeddedf <*> Just 5   -- :: Num a => Maybe (a -> a)

-- Apply <*> again to get the final result
result = embeddedf' <*> Just 10     -- Just 15

-- what about a -> b -> c -> d?
result2 = pure (,,) <*> Just 5 <*> Just 10 <*> Just 20  -- Just (5, 10, 20)

-- pure can be used to embed any value inside a Functor
result3 = pure "Hello" :: [String]      -- ["Hello"]
result4 = pure "Hello" :: Maybe String  -- Just "Hello"


{-
    With pure and <*>, we can apply functions taking any number of parameters
    to Functors, like

    pure f <*> x <*> y <*> ...

    where x, y ... are Functor instances, say Just 5, Just 10.

    Actually, for a functor instance x,

    pure f <*> x == fmap f x

    Therefore, Control.Applicative exports another function <$>, which is just
    fmap as an infix operator. Here is how it's defined:

    <$> :: (Functor t) => (a -> b) -> t a -> t b
    f <$> x = fmap f x

    See how it works:
-}
-- same as pure (++) <*> Just "Hello " <*> Just "Isaac"
result5 = (++) <$> Just "Hello " <*> Just "Isaac"   -- Just "Hello Isaac"


{-
    How it works on Functor [a] or [] a.

    For a list, the implementation is:

    instance Applicative [] where
        pure x = [x]
        fs <*> xs = [f x | f <- fs, x <- xs]

    See how it works.
-}
result6 = [(+5), (*2)] <*> [1, 2, 3]    -- [6, 7, 8, 2, 4, 6]

-- same as [(+2), (+3), (*2), (*3)] <*> [4, 5]
result7 = [(+), (*)] <*> [2, 3] <*> [4, 5]  -- [6,7,7,8,8,10,12,15]

-- same as [(++ "Hello, "), (++ "Hi, ")] <*> ["Isaac", "Chloe", "Andy"]
-- ["Hello, Isaac","Hello, Chloe","Hello, Andy","Hi, Isaac",
--  "Hi, Chloe","Hi, Andy"]
result8 = (++) <$> ["Hello, ", "Hi, "] <*> ["Isaac", "Chloe", "Andy"]


{-
    More on list Functors, ZipList

    As we know from above, [ f1, f2, ... fm ] <*> [ a1, a2, ... an]
    will produce a list of length m x n, as below:

    [ f1 a1, f1 a2, ..., fm an]

    However, sometimes we want to zip the list of functions with the list
    of values, so that we have:

    [f1 a1, f2 a2, ... fm am] (suppose m <= n)

    This is where ZipList functor comes in handy. It's defined as:

    instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

    To construct a ZipList, we do

    z = ZipList [...]         -- it takes a list, or
    z = pure x :: ZipList     -- an infinite list of value x

    To take the list value out of a ZipList, do

    list = getZipList z
-}
z = ZipList [1, 2..]
zf = ZipList [(+ 1), (* 3), (^ 5)]
result9 = getZipList $ zf <*> z

-- Since ZipList is a Functor, the <$> works similarly to a list.
z2 = ZipList [1, 2..]
zf2 = (*) <$> ZipList [2, 3..]  -- same as ZipList [(*2), (*3)..]
result10 = take 5 $ getZipList $ zf2 <*> z2 -- [2,6,12,20,30]


{-
    Life a function to operate on Functors.

    With the help of pure and <*>, we can apply function on Functors. E.g.,

    pure (:) <*> Just 3 <*> Just [5]    -- Just [3, 5]

    Here we introduce another untility function liftA2, which does a
    similar thing, apply a function that takes 2 parameters (a -> b -> c)
    on two Functor instances t a and t b, then returns another functor
    instance t c.

    liftA2 :: (Applicative t) => (a -> b -> c) -> t a -> t b -> t c

    Here is how it works.
-}
result11 = liftA2 (:) (Just 3) (Just [4])   -- Just [3, 4]

-- similarly, we got liftA3,
result12 = liftA3 (,,) (Just 3) (Just 4) (Just 5)   -- Just [3, 4, 5]


{-
    What about Functor IO?

    Here is how:

    instance Applicative IO where
        pure a = return a
        a <*> b = do
            f <- a
            x <- b
            return (f x)

    Namely, get a function out of the first IO action, get a value out of
    the second IO action, apply the function to the value, then wrap the
    result into another IO action (return).

    Let's see how it works with the below example. Say we want to get two
    lines and concat them into one. We could do:

    myAction :: IO String
    myAction = do
        a <- getLine
        b <- getLine
        return $ a ++ b

    Instead, we can do the below:
-}
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
-- same as this
-- myAction = pure (++) <*> getLine <*> getLine
