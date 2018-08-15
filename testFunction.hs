-- Try out function syntax
--


-- When we are not sure about what the function signature is, we can define
-- the function first then import into GHCI and use :t to see it.
luckyNumber :: (Eq a, Num a) => a -> String
luckyNumber x =
    if x == 7 then
        "You are lucky!"
    else
        "Too bad, out of luck."


-- Pattern matching to implement the same function
--
luckyNumber' :: (Eq a, Num a) => a -> String
luckyNumber' 7 = "You are lucky!"
luckyNumber' x = "Too bad, out of luck."


-- Pattern matcing using case statements
luckyNumber2 :: (Eq a, Num a) => a -> String
luckyNumber2 x =
    case x of
        7 -> "You are lucky!"
        _ -> "Too bad, out of luck."


-- If use :t to find the type, then it is (Eq a, Num a) => a -> a
-- But we know that n must be a integer, however Int is not enough,
-- therefore we use typeclass Integral to cover both Int and Integer.
--
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- Pattern matching to destructure a tuple parameter
--
-- Add two vectors
addVector :: (RealFloat a) => (a, a) -> (a, a) -> (a, a)
addVector (x1, y1) (x2, y2) =
    (x1 + x2, y1 + y2)


-- Implement our own head function for List
--
head' :: [a] -> a
head' [] =
    error "Call head on an empty list."     -- generate a run time error
head' (x : _) =
    x


-- Use @ in pattern matching
--
-- when using xs@(x: y: ys) to match, xs matches the whole
capital :: String -> String
capital "" =
    "Empty string, Ooops!"
capital all@(x:xs) =
    "First letter of " ++ all ++ " is " ++ [x]


-- Use guards
--
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You are underweight."
    | bmi <= 25.0 = "You are supposedly normal."
    | bmi <= 30.0 = "You are fat."
    | otherwise   = "You are a whale."      -- othresise is defined as True
                                            -- in Haskell


-- Use where
--
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= skinny = "You are underweight."
    | bmi <= normal = "You are supposedly normal."
    | bmi <= fat    = "You are fat."
    | otherwise     = "You are a whale."
    where
        bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)  -- destructure using
                                                    -- pattern matching


-- Use Let/in
--
-- Unlike where, which defines a set of bindings, Let/in is an expression by
-- itself.
--
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let
        sideArea = h * 2 * pi * r
        topArea = pi * r ^2
    in
        sideArea + 2 * topArea


-- Use Let/in (inline)
--
listA = ( let a = 100; b = 200 in a*b, let foo = "Hey "; bar = "there" in foo ++ bar )


-- Use Let/in (list comprehension)
--
-- Calculate and filter out those big BMIs.
--
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs =
    [ bmi | (weight, height) <- xs, let bmi = weight / height ^2, bmi >= 25.0 ]
