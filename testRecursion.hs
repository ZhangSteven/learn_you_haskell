-- Implement take
--
-- take' :: (Integral i, Ord i) => i -> [a] -> [a]
-- take' :: (Ord n, Num n) => n -> [a] -> [a]
--
-- The below compiles but does not work for negative n, why?
-- take' n xs =
--     case xs of
--         [] -> []
--         y : ys ->
--             if n > 0 then    -- when n <=0, this line fails
--                 y : take' (n-1) ys
--             else
--                 []

-- Code from the book. However, it doesn't work either
-- take' :: (Num i, Ord i) => i -> [a] -> [a]
-- take' n _
--     | n <= 0 = []   -- When n is negative, then line fails, why?
-- take' _ []     = []
-- take' n (x:xs) = x : take' (n-1) xs


-- Implement zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' fn listA listB =
    case (listA, listB) of
        ([], _) -> []
        (_, []) -> []
        (x:xs, y:ys) -> (fn x y) : (zipWith' fn xs ys)


-- Implement flip
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' fn =
    g
    where g x y = fn y x


-- A different implementation
--
-- Note the difference in function signature.
-- 1. flip' takes a function and returns another.
-- 2. flip2 takes a function and two parameters, then compute the result.
--
-- Therefore flip2 f x becomes a curried function that needs one more
-- parameter y to compute the result as f y x
--
flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f x y =
    f y x

-- Or just implement using a lambda function
flip3 :: (a -> b -> c) -> b -> a -> c
flip3 f =
    \x y -> f y x


-- Use List filter
largestDivisible = head (filter p [ 10000, 9999.. ])
    where
        p x =
            x `mod` 3829 == 0


-- Find the sum of all odd squares smaller than 10,000
--
-- takeWhile: take elements from a list until the predicate function
--              returns False.
--
-- Note that we use an infinite list [1,2..] here. This is OK because
-- Haskell is lazy, it won't produce the whole list before start filtering
-- or other actions, it produce and work on the elements on the fly.
sumOdds =
    sum (takeWhile (<10000) [ n^2 | n <- [1,2..], odd n ])


-- 角谷猜想
--
-- For n <- N, if n is odd, then next number is 3*n + 1, otherwise n/2.
-- Do this until the number becomes 1. Put all the intermediate results
-- into a list.
chain :: (Integral a) => a -> [a]
chain n
    | n == 1 = [1]
    -- Note: we cannot use n/2 because it produces a floating number,
    -- that will cause a type mismatch error in compile time.
    -- Use `div` instead, it is integer division.
    --
    -- | n `mod` 2 == 0 = n : (chain n/2)
    | n `mod` 2 == 0 = n : chain (n `div` 2)
    | otherwise = n : chain (3*n + 1)


-- Find out for all starting numbers between 1 and 100, how many chains
-- have length greater than 15?
numbers = [ x | x <- [1..100], length (chain x) > 15 ]


-- List of functions
listOfFunctions = map (*) [0..10]
result = (listOfFunctions !! 4) 5   -- retrieve the function at index 4
                                    -- and apply it to 5


-- folding a list
--
-- Whenever we traverse a list once, element by element, and then produce
-- some result base on that, then we can use fold.
--
-- square and sum the numbers in a list
sumSquare :: (Num n) => [n] -> n
sumSquare =
    foldl (\acc x -> acc + x^2) 0


-- Implement the elem function
elem' :: (Eq a) => a -> [a] -> Bool
elem' el list =
    foldl (\acc x -> acc || el == x) False list


-- Now let's fold a list from the right.
--
-- NOTE: the accumulator function is different from the left fold,
-- it is in the form (\x acc -> ...)
--
-- Let's implement map
map' :: (a -> b) -> [a] -> [b]
map' f list =
    foldr (\x acc -> f x : acc) [] list
