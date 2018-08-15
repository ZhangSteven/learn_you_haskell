-- Types and Type Classes

-- We can use :t on the interactive console to see the type of a value.
-- E.g., :t 'a' -> 'a' :: Char, where :: operator defines the type, and
-- Char is the type.
--
-- :t "Hello" -> "Hello" :: [Char], list of Char (same os String)
-- a :: String is the same as a :: [Char]
--
-- Define a function to remove non uppercase characters
removeNoneUpperCase :: String -> String
removeNoneUpperCase s =
    [ c | c <- s, c `elem` [ 'A'..'Z' ] ]


-- Integral number types.
--
-- Int: a bounded integer. In a 64 bit machine, an Int is in the range of
-- [-9223372036854775808, 9223372036854775807]. Call "maxBound :: Int" and
-- "minBound :: Int" to find out that.
--
-- Integer: an unbounded integer, but not as efficient as Int.
--
-- Had we used "Int" as the type, factorial 100 will return 0, because it
-- is out of bound.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- Floating point number types.

-- Float/Double : both floating point numbers, but Double provides double
-- the precision.
--
-- circumference :: Float -> Float
circumference :: Double -> Double       -- precission increases a lot
circumference r =
    2 * pi * r


--
-- Typeclasses: a Typeclass is a sort of interface that defines some behaviour.
-- If a type is part of a typeclass, that means that it supports and implements
-- the behaviour the typeclass describes.
--
-- Let's find out the type signature of the "==" function.
--
-- *Main> :t (==)
-- (==) :: Eq a => a -> a -> Bool
--
-- Here "Eq a" means type variable a must belong to typeclass "Eq". We can pass
-- a Float, an Int, a Char or a String etc. to the function "==" because those
-- types belong to typeclass "Eq".
--
-- Typeclass : Ord
--
-- *Main> :t compare
-- compare :: Ord a => a -> a -> Ordering
--
-- Ord is a typeclass requiring a type to be orderable. Float/Double/Int/Char/
-- String all belong to Ord typeclass. Ordering is a type with 3 values: GT,
-- EQ and LT.
--
-- Typeclass : Show
--
-- *Main> :t show
-- show :: Show a => a -> String
--
-- show is a function that converts a value to String. It requires that value
-- to be "showable", i.e., convertable to String. So far, except for functions,
-- all values are convertable to String.
--
-- Typeclass : Read
-- *Main> :t read
-- read :: Read a => String -> a
--
-- *Main> read "3.14" + 5.5     -- compiler infers the return type as Float
-- 8.64
--
-- *Main> read "3.14"
-- *** Exception: Prelude.read: no parse
--
-- *Main> read "3.14" :: Float  -- specify the return type explicitly
-- 3.14
--
-- Typeclass : Num (any number), Fractional (any floating point number, real or
-- complex)
--
-- See the type of expression "20" and "3.14":
-- *Main> :t 20
-- 20 :: Num t => t
--
-- *Main> :t 3.14
-- 3.14 :: Fractional t => t
--
-- *Main> :t (20 + 3.14)
-- (20 + 3.14) :: Fractional a => a
--
-- *Main> 20 :: Int + 3.14   -- error: inconsistnet type
