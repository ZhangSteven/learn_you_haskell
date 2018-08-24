{-
    Numbers Game.

    From chapter 9, programming in Haskell

    Starting from a list of natural numbers, find an expression with
    add, subtract, multiple, divide and parenthese to reach the target
    value.

    This version uses the approach of evaluating expressions while
    generating them, filtering out those invalid expressions in the
    generation phase.
-}
main :: IO ()
main =
    print $ solutions [1, 3, 7, 10, 25, 50] 842


-- The allowed operations we can have
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- The list of all operators
allOps :: [Op]
allOps = [Add, Sub, Mul, Div]

-- represent an expression
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val a) = show a
    show (App op x y) =
        "(" ++ show x ++ show op ++ show y ++ ")"


{-
    Evaluate an expression.

    Here we use a list to denote the result, where

    1. A singleton list denotes a successful result.
    2. An empty list denotes failure.

    We can also use Maybe Int, but we prefer list because the list comprehension
    provides a convenient way to define the eval function.
-}
eval :: Expr -> [Int]
eval (Val n) = [ n | n > 0 ]
eval (App op left right) =
    [ apply op x y | x <- eval left, y <- eval right, valid op x y ]


-- Will the expression yield a valid result? Only positive result are
-- valid.
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0


-- apply the operator to inputs
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


{-
    Sub lists of a list

    subs [1]       = [[1]]
    subs [1, 2]    = [[],[1],[2],[1,2]]
    subs [1, 2, 3] = [[],[1],[2],[3],[2,3],[1,3],[1,2],[1,2,3]]
-}
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) =
    {- Sub lists without x merged with sublists with x -}
    yss ++ map (x :) yss where
        yss = subs xs


{-
    Interleave: all possible ways of inserting an element into a list.

    interleave 1 []     = [[1]]
    interleave 1 [2]    = [[1,2],[2,1]]
    interleave 1 [2, 3] = [[1,2,3],[2,1,3],[2,3,1]]
-}
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) =
    (x:y:ys) : map (y :) (interleave x ys)


{-
    Permutations of a list

    perms [1] = [[1]]
    perms [1, 2] = [[1,2],[2,1]]
    perms [1, 2, 3] = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) =
    concatMap (interleave x) (perms xs)


{-
    Choices from a list: Permutations of all sub lits.

    choices [1] = [[], [1]]
    choices [1, 2] = [[],[2],[1],[1,2],[2,1]]
    choices [1, 2, 3] = [[],[1], [2], [3],
                        [2,3],[3,2],[1,3],[3,1],[1,2],[2,1],
                        [1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
choices :: [a] -> [[a]]
choices = concat . map perms . subs


{-
    Instead of generating all possible expressions from a list of numbers and
    evaluating them later, we evalute expressions while generating them. This
    way, we filter out those invaliad expressions at early stage and each
    expression is evaluated only once.
-}
type Result = (Expr, Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)]
results xs =
    let
        {-
            Split a list into pairs of sub lists, where each sub list must be
            non-empty and the orders of list elements unchanged.

            split [1, 2] = [([1],[2])]
            split [1, 2, 3] = [([1],[2,3]),([1,2],[3])]
        -}
        split :: [a] -> [([a], [a])]
        split [] = []   -- doesn't make sense to split an empty list
        split [_] = []  -- doesn't make sense to split a singleton list.
        split (x:xs) =
            ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

        combine :: Result -> Result -> [Result]
        combine (el, rl) (er, rr) =
            [ (App op el er, apply op rl rr) | op <- allOps, valid op rl rr ]

    in
        [ (e, r) | (ls, rs) <- split xs
                 , (el, rl) <- results ls
                 , (er, rr) <- results rs
                 , (e, r) <- combine (el, rl) (er, rr) ]


solutions :: [Int] -> Int -> [Expr]
solutions ns target =
    [ e | ns' <- choices ns, (e, r) <- results ns', r == target ]
