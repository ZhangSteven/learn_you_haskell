{-
    Functor and Applicative Functor.

    From:

    Learn you a haskell

    Programming in haskell
-}


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

    We are going to need to define yet another version, say fmap3 to solve
    this problem.

    Therefore, we are going to need unlimited number of version of fmap's in
    the definition of Functor typeclass.

    To solve this problem, we define two extra functions:

    pure :: (Functor t) => a -> t a

    with pure, we can embed a function inside a Functor type, say, Just (+).

    Another function is to apply function embedded a Functor instance to value
    embedded in another Functor instance:

    <*> :: (Functor t) => t (a -> b) -> t a -> t b

    Functors supporting pure and <*> functions are called Applicative Functor,
    a subset of Functor typeclass.

    See how it works:
-}
embeddedf = pure (+) -- pure (+) :: (Num a, Applicative f) => f (a -> a -> a)

-- Apply once to become a curried function
embeddedf' = embeddedf <*> Just 5   -- :: Num a => Maybe (a -> a)

-- Apply again to get the final result
result = embeddedf' <*> Just 10     -- Just 15


-- what about a -> b -> c -> d?
result2 = pure (,,) <*> Just 5 <*> Just 10 <*> Just 20  -- Just (5, 10, 20)

{-
    Therefore, we can see that with pure and <*>, we can apply functions taking
    any number of parameters to Functors. In Haskell, Maybe a, [a], Either b a
    are already Applicative Functor.
-}
