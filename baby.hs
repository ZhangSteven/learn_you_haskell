doubleMe x =
    x + x + x


doubleSmallNumber x =
    if x > 100 then
        x
    else
        x * 2


-- Note: ' is a valid character in function name
-- A slight different version from doubleSmallNumber
doubleSmallNumber' x =
    let
        y =
            if x > 100 then
                x
            else
                x * 2
    in
        y + 1


-- List comprehensions
--
-- Find all even numbers in a range xs, which has a reminder of 3 when
-- divided by 7
findNumber xs =
    [ x | x <- xs, even x, x `mod` 7 == 3 ]


--
-- Find all right triangles which
-- 1. has integers for all sides,
-- 2. all sides equal to or smaller than 10,
-- 3. has a perimeter of 24
--
-- List comprehension can help us to solve the problem.
--
-- Unlike in an imperative language, where we use loops to iterate over
-- a range and find out the possible solutions. Here we only specify the
-- range and the predicate (filters) in a list comprehension, then the
-- resulting list will contain the solution.

rightTriangles = [ (a, b, c)
                    | c <- [1..10]  -- specify range
                    , b <- [1..c]
                    , a <- [1..b]
                    , a^2 + b^2 == c^2  -- specify predicate (filters)
                    , a + b + c == 24
                 ]      -- [ (6, 8, 10) ]
