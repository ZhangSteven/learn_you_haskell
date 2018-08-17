{-
    Define data structures recursively.

    Some data structures are recursive in nature. For example, a list can be
    thought of as a head plus the tail, which is another list.

    Therefore, we can define a list as:
-}

data List a =
    Empty               -- constructor for the edge case, an empty list
    | Cons a (List a)   -- constructor for a normal case, a head of type a
                        -- plus a tail of type List a.
    deriving Show

{-
    Test on GHCI:

    *Main> list = Empty
    *Main> :t list
    list :: List a
    *Main> list2 = Cons (5 :: Int) Empty
    *Main> :t list2
    list2 :: List Int
    *Main> list3 = Cons 5.5 Empty
    *Main> :t list3
    list3 :: Fractional a => List a
    *Main> list4 = Cons 4 Empty
    *Main> :t list4
    list4 :: Num a => List a
    *Main> list = Cons 5.5 list4
    *Main> list
    Cons 5.5 (Cons 4.0 Empty)
    *Main> :t list
    list :: Fractional a => List a
-}


{-
    Use a constructor with infix style.

    Let's create an infix function :-: (infix function consists of only special
    characters). Then we define its associativity and fixity.

    The function needs to be right associative, namely,

    3 :-: 4 :-: Empty

    is the same as,

    3 :-: (4 :->: Empty)

    fixity defines the priority level.
-}

infixr 5 :-:       -- infixr means right associative, 5 is fixity

data List2 a =
    EmptyList
    | a :-: (List2 a)
    deriving Show

-- Define the list concat operator
-- NOTE: we used pattern matching (x :-: xs) here, pattern matching is actually
-- about matching constructors
infixr 5 .++
(.++) :: List2 a -> List2 a -> List2 a
(.++) xs ys =
    case (xs, ys) of
        (EmptyList, ys) -> ys
        (x :-: xs, ys) -> x :-: (xs .++ ys)


{-
    Create a binary tree.

    Just like a list, a binary tree can also be defined recursively. Each node
    in a tree contains the following∷

    1. a value of some type.
    2. up to two sub nodes (or one, or none).
-}
data Tree a =
    EmptyTree                   -- edge case, no nodes at all
    | Node a (Tree a) (Tree a)  -- a node containing some value, and two sub
                                -- nodes (they can be EmptyTree)
    deriving (Show, Eq)


-- functions to create a tree from a value
singleton :: a -> Tree a
singleton x =
    Node x EmptyTree EmptyTree


{-
    Insert a value into a tree.

    Return a new tree with the below operation: starting from the root node,

    1. If the node is empty, then replace the node with a new node containing
        the value.

    2. If the node is not empty, then,

        2.1 if the value is equal to the value in the node, just return the
            tree unchanged.

        2.2 if the value is smaller than the value in the root node, then go to
            the left sub node and repeat 1.

        2.3 If the value is bigger than the value in the root node, then go to
            the right sub node and repeat 1.

    Since we are making comparisons, ==, > and <, then we need to a type
    constraint (Ord a) to the function signature.
-}

insert :: (Ord a) => a -> Tree a -> Tree a
insert x tree =
    case tree of
        EmptyTree -> Node x EmptyTree EmptyTree
        Node value leftTree rightTree ->
            if x == value then
                tree
            else if x < value then
                Node value (insert x leftTree) rightTree
            else
                Node value leftTree (insert x rightTree)


-- The solution from the book, use guards to replace if then else
insert2 :: (Ord a) => a -> Tree a -> Tree a
insert2 x EmptyTree = singleton x
insert2 x (Node value leftTree rightTree)
    | x == value = Node value leftTree rightTree
    | x < value = Node value (insert2 x leftTree) rightTree
    | x > value = Node value leftTree (insert2 x rightTree)


-- Test whether a value is contained by a node in the tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node value leftTree rightTree)
    | x == value = True
    | x < value = treeElem x leftTree
    | x > value = treeElem x rightTree


-- Build a tree from a list
fromList :: (Ord a) => [a] -> Tree a
fromList xs =
    -- foldl (\acc x -> insert2 x acc) EmptyTree xs
    -- there is a better one:
    foldl (flip insert2) EmptyTree xs


{-
    Type class again.

    As we mentioned before, a typeclass defines interfaces that must be
    implemented by its member types. For example, the "Eq" typeclass's
    definition looks like:

    typeclass Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)

    The last two expressions are there to help, so that we only need to
    implement either (==) or (/=) for a type for it be in typeclass Eq,
    it is called minimal complete definition for the typeclass.

    Now let's try to define our own typeclass and see how it works.
-}

class YesNo a where
    yesNo :: a -> Bool

-- Now let's define some concrete types for that class.
instance YesNo Int where
    yesNo 0 = False
    yesNo _ = True

instance YesNo [a] where
    yesNo [] = False
    yesNo _ = True

instance YesNo Bool where
    yesNo = id      -- id is the identity function, its' the same as
                    -- yesNo x = id x

instance YesNo (Maybe a) where
    yesNo Nothing = False
    yesNo _ = True

-- Let's define our own type and put it into class YesNo
data TrafficLight =
    Red
    | Yellow
    | Green

-- Put Traffic into Eq typeclass. Instead of "deriving Eq", we manually
-- implement the "==" function.
instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

-- Put TrafficLight into YesNo typeclass
instance YesNo TrafficLight where
    yesNo Red = False
    yesNo _ = True


yesNoIf :: (YesNo a, Show b) => a -> b -> b -> b
yesNoIf value yesResult noResult =
    if (yesNo value) then
        yesResult
    else
        noResult

{-
    Try it out:

    yesNoIf 0 "ok" "Ooops" : Won't work, because 0 is an expression of
    typeclass (Num t => t), meaning 0 can be converted to a concrete type
    depending on the context, say∷

    0 + 5 :: Int    -- 0 will be converted to Int, and the result of type Int
    0 + 5.5         -- 0 will be converted to typeclass Fractional

    The below is OK:

    *Main> yesNoIf (0::Int) "ok" "Ooops"
    "Ooops"
    *Main> yesNoIf [] "ok" "empty list"
    "empty list"
    *Main> yesNoIf [0] "ok" "Ooops"
    "ok"
    *Main> yesNoIf Yellow "ok" "cannot go through"
    "ok"
-}


{-
    The Functor typeclass

    From previous examples, we have defined typeclass for concrete types, now
    let's look at typeclass for parameterized types. E.g., for [a], Maybe a,
    etc.

    Since typeclass works like an interface, defining functions so that each
    instance of the typeclass needs to implement, we can view those functions
    as polymorphic functions. For example, we can define "==" for a type that
    is an instance of Eq type class, then when calling == on that type, the
    function will be called.

    For many parameterized types, like [a] or Maybe a, what if we want to
    use a function to change the value inside it? Say map Maybe a to Maybe b?

    In this case, Haskell defines a typeclass called Functor, as below∷

    class Functor f where
        fmap :: (a -> b) -> f a -> f b

    where f is a parameterized type constructor, like [] or Maybe. In Haskell,
    Maybe is an instance of the Functor typeclass. So that when we call:

    fmap (+1) Just 5        -- Just 6
    fmap (+1) Nothing       -- Nothing

    Then Haskell will call the fmap defined by Maybe type contructor to handle
    the above call.

    It turns out that [] is also an instance of the Functor typeclass.
    When we do :

    fmap (+1) [1, 2, 3]     -- [2, 3, 4]
    fmap (+1) []            -- []

    Then Haskell will call the fmap defined by [] type constructor to handle
    the above call. When we look into the source code, it's actually just

    fmap = map      -- in the case of []
-}


{-
    Try "[]" as the type constructor

    In GHCI, we can define a list of Int as below:

    *Main Map> m = [5] :: [Int]
    *Main Map> :t m
    m :: [Int]

    -- Or, we can do it this way, using [] as type constructor
    *Main Map> m = [5] :: ([] Int)
    *Main Map> :t m
    m :: [Int]

    Let's implement fmap for [a], or [] a.
-}
