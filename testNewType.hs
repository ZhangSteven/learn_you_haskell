{-
    NewType keyword.

    We can define data types in 3 ways:

    1. Type alias: type Result = Int
    2. New data type: data Size = Small | Big | Medium
    3. New data type: newtype ...

    The difference between data and newtype is that:

    1. It has only one constructor and that constructor can take only one
        value.
    2. newtype is quicker than data.

    Let's take an example.
-}

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
clist = CharList "Hello"
clist2 = CharList ['H', 'e', 'l', 'l', 'o']
clist3 = CharList { getCharList = "Hello" }
result = (clist == clist2)  -- True
result2 = (clist == clist3)

-- Since we use the record syntax, getCharList becomes an auto-generated func
body = getCharList clist    -- "Hello"


{-
    An example: can we make tuple (a, b) an instance of Functor, so that
    fmap f (a, b) = (f a, b)

    Let's use newtype solve the problem.
-}
newtype Pair b a = Pair { getPair :: (a, b) } deriving (Show)

instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

pair = Pair (20, "hello")
result3 = fmap (*2) pair    -- (40, "hello")
