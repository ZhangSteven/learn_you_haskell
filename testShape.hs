{-
    Test import the Shape module

    QUESTION: when Shape.hs exposes baseCircle without Shape data type,
    the below code still works in GHCI. In GHCI, we see∷

    *Main> c
    Circle (Point (-5.0) 5.0) 10.0
    *Main> :t c
    c :: Shape.Shape

    So it's OK not to export the data type?
-}
import Shape

-- create a Circle
c = nudge (baseCircle 10) (-5) 5
areaC = area c
