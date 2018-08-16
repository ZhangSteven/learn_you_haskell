{-
    Create our own module
-}
module Geometry
( sphereVolumn
, sphereArea
, cubeVolumn
, cubeArea
, cuboidVolumn
, cuboidArea
) where


sphereVolumn :: Float -> Float
sphereVolumn r =
    -- This doesn't compile because '^' expects Integral numbers, use **
    -- for floating numbers
    -- (4.0 / 3.0) * pi * r^3
    (4.0 / 3.0) * pi * r**3


sphereArea :: Float -> Float
sphereArea r =
    4.0 * pi * r**2


cubeVolumn :: Float -> Float
cubeVolumn side =
    cuboidVolumn side side side


cubeArea :: Float -> Float
cubeArea side =
    cuboidArea side side side


cuboidVolumn :: Float -> Float -> Float -> Float
cuboidVolumn a b c =
    rectangleArea a b * c


cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c =
    2 * (rectangleArea a b + rectangleArea b c + rectangleArea c a)


rectangleArea :: Float -> Float -> Float
rectangleArea a b =
    a * b
