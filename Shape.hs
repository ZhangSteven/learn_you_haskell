{-
    Create our own data types
-}
module Shape
( Point (..)
, Shape (..)
, area
, nudge
, baseCircle
, baseRect
) where


-- Point: position of a point
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

-- NOTE: when we derive Eq, we need to make sure each member in that record
-- type belongs to typeclass Eq.


{-
    Haskell can order the values of our own data type if we define the
    type as Ord.
-}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)
