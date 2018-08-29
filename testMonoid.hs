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
    a list structure [a] is a Monoid instance defined as follows:

    instance Monoid [a] where
        mempty = []
        mappend = (++)

    let's try it out:
-}
import Data.Monoid
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F


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
        mempty = Sum 0
        Sum x `mappend` Sum y = Sum (x + y)

    Let's try:
-}
result3 = mempty :: (Num t) => Product t    -- Product 1
result4 = mempty :: (Num t) => Sum t        -- Sum 0

result5 = getProduct . mconcat . map Product $ [2, 3, 4]    -- 24 (2 * 3 * 4)
result6 = getSum .mconcat . map Sum $ [2, 3, 4]             -- 9 (2 + 3 + 4)


{-
    How to sum a list of Maybes : [Maybe a]

    There are two ways to define the sum:

    1. Treat Nothing as failure, i.e., if Nothing appears in the list, then
        the sum result is Nothing.

        sumMaybe :: (Num a) => [Maybe a] -> Maybe a

        sumMaybe [Just 1, Just 2] -> Just 3
        sumMaybe []               -> Just 0
        sumMaybe [Nothing]        -> Nothing

    2. Ignore Nothing, i.e., only sum up those successful values.

        sumMaybe2 :: (Num a) => [Maybe a] -> a

        sumMaybe2 [Just 1, Just 2, Nothing] -> 3
        sumMaybe2 []                        -> 0
        sumMaybe2 [Nothing]                 -> 0
-}

sumMaybe :: (Num a) => [Maybe a] -> Maybe a
sumMaybe =
    foldl (liftA2 (+)) (Just 0)

-- Or, use sequence [Maybe a] -> Maybe [a]
sumMaybe' :: (Num a) => [Maybe a] -> Maybe a
sumMaybe' list =
    sum <$> sequence list

-- this version treats Nothing as 0.
sumMaybe2 :: (Num a) => [Maybe a] -> a
sumMaybe2 =
    -- Use Monoid type Sum, this works
    -- getSum . mconcat . map (maybe mempty Sum)
    --
    -- But there is a simpler form
    sum . map (fromMaybe 0)


list1 = [Just 1, Just 2]
list2 = []
list3 = [Just 1, Nothing]
resultSum = [sumMaybe, sumMaybe'] <*> [list1, list2, list3] -- [Just 3,Just 0,N]
resultSum2 = map sumMaybe2 [list1, list2, list3]    -- [3, 0, 1]


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


{-
    Monoids to help define a Foldable structure. A Foldable structure t a
    does not have to contain Monoid types. But if they do, we can use the
    following function to make our data type a Foldable type.

    foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

    It takes a function mapping type a to type m (Monoid), a Foldable
    structure whose value type is a, then reduce everything to a single
    value of type m.

    Let's take the example of a Tree, how to make it a Foldable instance.
-}
data Tree a = Empty | Node a (Tree a) (Tree a)
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x leftTree rightTree) =
        foldMap f leftTree `mappend`
        f x                `mappend`
        foldMap f rightTree


tree :: Tree Int
tree = Node 5
        (
            Node 3
                (Node 2 Empty Empty)
                (Node 4 Empty Empty)
        )
        (
            Node 7
                (Node 6 Empty Empty)
                (Node 8 Empty Empty)
        )

{-
    Try it out.

    Here the function Sum/Product determines the mempty and mappend used
    in foldMap.
-}
resultTree1 = getSum $ foldMap Sum tree             -- 35
resultTree2 = getProduct $ foldMap Product tree     -- 40320

-- Since it's a Foldable structure now, we can call F.foldl foldr now.
-- QUESTION: how is F.foldl and foldr related to foldMap?
resultTree3 = F.foldl (+) 0 tree    -- 35
resultTree4 = F.foldl (*) 1 tree    -- 40320
resultTree5 = F.foldl (-) 10 tree   -- (-25), or 10 - sum (tree)


{-
    With the foldMap on Tree defined, we can do some more interesting
    things.

    Check whether any element is equal to 3. Here we use another Monoid
    type Any, like Sum, it has the following definition:

    newtype Any = Any { getAny :: Bool }

    instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)

    All is like Any, except that All has mempty = True and mappend =
    All (x && y).
-}
resultTree6 = getAny $ foldMap (\x -> Any $ x == 3) tree    -- True
resultTree7 = getAll $ foldMap (\x -> All $ x > 3) tree     -- False

-- Let's try the list Monoid, convert the tree to list
resultTreeList = foldMap (\x -> [x]) tree       -- [2, 3, 4, 5, 6, 7, 8]
