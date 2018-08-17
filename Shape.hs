{-
    Create our own data types

    From the code below, there are two ways to generate a Shape value,
    1. Use the constructor, e.g., Circle Point (x, y) r.
    2. Use the baseCircle and baseRect to create a Shape and then call
        nudge function to move it to a desired location.

    In approach 1, we need to expose the Shape, Point data type and their
    constructors, e.g.,

    module Shape
    ( Point(..)
    , Shape(..)
    ...)

    In approach 2, we can just expose the Shape data type without the
    constructor, e.g.,

    module Shape
    ( Shape
    , ...
    )

    See testShape.hs for the testing code.
-}
module Shape
-- ( Point (..)    -- expose the data type as well as its constructors
-- , Shape (..)
-- ( Shape
( area
, nudge
, baseCircle
, baseRect
) where
import qualified Data.Map as Map


{-
    use "data" to create a new type.

    Point: position of a point

    NOTE: "Point" on the left hand side defines the type name, while "Point" on
    the right hand side defines a constructor function used to create the type.
    These two names happen to be the same just for convenience, they can be
    different, e.g.,

    data Point = DoPoint Float Float

    In this case, "Point" is the data type name and "DoPoint" is the
    constructor.
-}
data Point = Point Float Float deriving Show


{-
    Define the Shape data type.

    Note: we put "deriving Show" at the end of the data type declaration,
    so that this data type becomes showable, i.e., can be converted to
    String.
-}
data Shape = Circle Point Float     -- position of the center and radius
            | Rectangle Point Point -- position of the bottom left and upper
                                    -- right corner
            deriving Show


-- Area of different shapes
area :: Shape -> Float
area (Circle _ r) =
    circleArea r
area (Rectangle (Point x1 y1) (Point x2 y2)) =
    rectangleArea (abs $ x2-x1) (abs $ y2-y1)


circleArea :: Float -> Float
circleArea r =
    pi * r**2


rectangleArea :: Float -> Float -> Float
rectangleArea height width =
    height * width


-- Move a Shape to somewhere else based on the distance (a, b)
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b =
    Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


-- We can create shapes at position (0, 0) and later move them
baseCircle :: Float -> Shape
baseCircle =
    Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height =
    Rectangle (Point 0 0) (Point width height)


{-
    Use records

    The members "_firstName" etc. are functions as well, therefore to
    avoid name clash with other functions, we prepend them with "_"
-}
data Person =
    Person { _firstName :: String
           , _lastName :: String
           , _age :: Int
           , _height :: Float
           , _phoneNumber :: String
           , _flavor :: String
           } deriving Show


-- let's create a value of type Person
p :: Person
p = Person { _firstName = "Isaac"
           , _lastName = "Zhang"
           , _age = 6
           , _height = 1.14
           , _phoneNumber = "1234567"
           , _flavor = "Chocolate" }


{-
    Parameterized types
-}
data Vector a =
    Vector a a a deriving Show

vPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector a b c) `vPlus` (Vector x y z) =
    Vector (a+x) (b+y) (c+z)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector a b c) `vectMult` k =
    Vector (a*k) (b*k) (c*k)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector a b c) `scalarMult` (Vector x y z) =
    a*x + b*y + c*z


{-
    Derived type instances

    Haskell has typeclasses, such as Eq, Ord, etc. They are interfaces
    requiring certain functionality. Int, String are concrete types
    that belong to one or more typeclasses.

    When we define our own data type, we can make them into certain
    type typeclasses with the keyword deriving.
-}
data SimplePerson = SimplePerson { firstName :: String
                                 , lastName :: String
                                 , age :: Int
                                 } deriving (Eq, Show, Read)


-- Use the value contructor SimplePerson to create a value of type SimplePerson
isaac = SimplePerson "Isaac" "Zhang" 6

-- NOTE: when we derive Eq, we need to make sure each member in that record
-- type belongs to typeclass Eq.


{-
    Another example, define a Day data type which can be ordered (Ord), can be
    enumerated (Enum).
-}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

days = [Monday .. Sunday]   -- Enum


{-
    Type synonyms

    Rather than defining a new type with keyword "data", we can create an alias
    to an existing data type.
-}

-- Instead of doing the below:
-- phoneBook :: [ (String, String) ]
--
type PhoneNumber = String
type Name = String
type PhoneBook = [ (Name, PhoneNumber) ]

phoneBook :: PhoneBook
phoneBook = [ ("betty", "555-2938")
            , ("bonnie", "452-2928")]


{-
    Parameterized type synonyms

    In the above, PhoneBook etc. are concrete types, but sometimes we would
    like to have parameterized types, like a Map whose key type is Int
    but value type unknown.

    type IntMap a == Map Int a

    Note "Map Int Int" defines a concrete type, however, partially applying
    to type "Map" results in parameterized type. Therefore we can also do:

    type IntMap = Map Int

    Unlike define PhoneBook as [ (String, String) ], we can define an
    associative array as below:
-}
type AssocList k v = [ (k, v) ]

ages :: AssocList String Int
ages = [ ("Issac", 6) ]


{-
    Widely used data type: Either

    It's parameterized type roughly defined as:

    data Either a b =
        Left a
        | Right b
        deriving (Eq, Ord, Read, Show)

    It can be used to represent two different types of values, such as a
    query result, type a represent the error and type b represents the
    successful result.

    Here is an example.
-}
data LockerState =
    Taken
    | Free
    deriving Eq     -- need this to make the "state == Taken" check below

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)    -- map locker number to
                                                    -- locker status and code


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist."
        Just (state, code) ->
            if state == Taken then
                Left $ "Locker " ++ show lockerNumber ++ " is taken."
            else
                Right code


lockerMap :: LockerMap
lockerMap = Map.fromList [ (100, (Taken, "CTYNE"))
                         , (101, (Free,  "NY74N"))
                         , (102, (Taken, "C61M6")) ]

-- Try lookup
result1 = lockerLookup 100 lockerMap
result2 = lockerLookup 200 lockerMap
result3 = lockerLookup 101 lockerMap
