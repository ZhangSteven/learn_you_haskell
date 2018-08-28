{-
    Monoid is a term from abstract algebra, a set S is a monoid if it satisfies
    the following conditions:

    1. There exists a binary operator OP, so that for all a, b, c in S, the
        following equation holds:

        (a OP b) OP c = a OP (b OP c)

    2. There exists an element e in S, so that for any element x in S,

        e OP x = x OP e = x

    Let's see how the Monoid type class is defined in Haskell.

    class Monoid m where
        mempty :: m                 -- the identity element
        mappend :: m -> m -> m      -- the binary operator

        mconcat :: [m] -> m
        mconcat = foldr mappend mempty

    We can see that Monoid class instances are concrete types, unlike
    Functor instances are parameterized types. Each Monoid instance has
    its own mempty constant and mappend binary operator. For example,
    [a] is a Monoid instance defined as follows:

    instance Monoid [a] where
        mempty = []
        mappend = (++)

    let's try it out:
-}
import Data.Monoid


-- for list, concat [a] == mconcat [a]
result = mconcat [ [1, 2], [3], [4, 5, 6, 7] ]  -- [1,2,3,4,5,6,7]
result2 = mempty :: [a]                         -- []


{-
    Besides [a], Num is another Monoid type, but it has two sets of
    binary functions:

    1. binary function (+), identity element 0
    2. binary function (*), identity element 1

    Therefore, Haskell defines two Monoid types for them, in Data.Monoid,

    newtype Product a = Product { getProduct :: a }
        deriving (Eq, Ord, Read, Show, Bounded)

    instance (Num a) => Monoid (Product a) where
        mempty = Product 1
        Product x `mappend` Product y = Product (x * y)

    newtype Sum a = Sum { getSum :: a }

    instance (Num a) => Monoid (Sum a) where
        mempty = Sum 1
        Sum x `mappend` Sum y = Sum (x + y)

    Let's try:
-}
result3 = mempty :: (Num t) => Product t    -- Product 1
result4 = mempty :: (Num t) => Sum t        -- Sum 0

result5 = getProduct . mconcat . map Product $ [2, 3, 4]    -- 24 (2 * 3 * 4)
result6 = getSum .mconcat . map Sum $ [2, 3, 4]             -- 9 (2 + 3 + 4)


{-
    Ordering as Monoid.

    There are 3 values in the type Ordering: LT, GT, EQ

    It turns out that EQ is the identity value, and the binary function
    is defined as follows:

    instance Monoid Ordering where
        mempty = EQ
        LT `mappend` _ = LT
        EQ `mappend` x = x
        GT `mappend` _ = GT

    We can see that x `mappend` EQ = x and EQ `mappend` x = x.

    So, for the expression of a series of `mappend`,

    x `mappend` y `mappend` z ...

    if x is not EQ, then x determines the result, otherwise it depends on y
    if y is not EQ, otherwise it depends on z, and so on.

    This property is very useful. Let's take an example.

    We want to compare two strings, based on the below criteria:

    1. The longer string is bigger. If they are of the same length, then
    2. The one with more vowel letters is bigger, if this is the same, then
    3. The call compare on them.
-}
vowels :: String -> Int
-- vowels = length . filter (flip elem "aeiou")     -- same as below
vowels = length. filter (`elem` "aeiou")


stringCompare :: String -> String -> Ordering
stringCompare s1 s2 =
    let
        x = length s1 `compare` length s2
        y = vowels s1 `compare` vowels s2
    in
        if x == EQ then
            if y == EQ then
                s1 `compare` s2
            else
                y
        else
            x


-- We now have a much more compact solution using the Monoid Ordering
-- Put the most import condition as the first, the second most important
-- as the second, and so on.
stringCompare2 :: String -> String -> Ordering
stringCompare2 s1 s2 =
    (length s1 `compare` length s2) `mappend`
    (vowels s1 `compare` vowels s2) `mappend`
    (s1 `compare` s2)


{-
    Maybe a as Monoid

    There are various ways that Maybe a can be an instance of Monoid. For
    example, if a is of type Monoid, then,

    instance (Monoid a) => Monoid Maybe a where
        mempty = Nothing
        m `mappend` Nothing = m
        Nothing `mappend` m = m
        Just x `mappend` Just y = Just (x `mappend` y)
-}
result7 = Just [1, 2] `mappend` Just [3, 4, 5]  -- Just [1, 2, 3, 4, 5]
result8 = Just "Hello " `mappend` Just "Isaac"  -- Just "Hello Isaac"
result9 = Just (Product 8) `mappend` Just (Product 4)   -- Just (Product 32)


{-
    Maybe a as Monoid (2)

    Besides the above, there is another way to make Maybe a
    an instance of Monoid, regardless whether a is an instance of Monoid.

    instance Monoid Maybe a where
        mempty = Nothing
        m `mappend` Nothing = m
        Nothing `mappend` m = m
        Just x `mappend` Just y = Just x

    This definition also satisfies the Monoid conditions. Here are
    some examples.

    Just like using Product and Sum to represent two ways of making numbers
    an instance of Monoid, here we use First to represent Maybe.

    newtype First a = First { getFirst :: Maybe a }
    instance Monoid First a where
        mempty = First Nothing
        First Nothing `mappend` m = m
        First (Just x) `mappend` _ = First (Just x)
-}
result10 = getFirst $ First (Just 'a') `mappend` First (Just 'b')
result11 = getFirst $ First Nothing `mappend` First (Just 'b')

-- Get the first non-Nothing value (Just 10)
result12 = getFirst $ mconcat $ map First [Nothing, Just 10, Just 20, Nothing]


{-
    Maybe a as Monoid (3)

    Just like First, if we modify the mappend function to always keep the
    second argument, as below

    newtype Last a = Last { getLast :: Maybe a }
    instance Monoid Last a where
        mempty = Last Nothing
        m `mappend` Last Nothing = m
        _ `mappend` Last (Just x) = Last (Just x)
-}
result13 = getLast $ Last (Just 'a') `mappend` Last (Just 'b')
result14 = getLast $ Last (Just 'a') `mappend` Last Nothing

-- Get the last non-Nothing value (Just 20)
result15 = getLast $ mconcat $ map Last [Nothing, Just 10, Just 20, Nothing]
