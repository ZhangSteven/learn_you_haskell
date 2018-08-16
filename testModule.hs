{-
    Let try some really cool functions from Data.List module.

    Some frequently used functions, filter, map are already loaded
-}
-- use the "quanlified" keyword, so that functions from this package must
-- be called with package name, like Data.Map.foldr. We also use the "as"
-- keyword, then we can use "Map.foldr" instead. 
import qualified Data.Map as Map


-- whether any element in a list satisfy the predicate function.
numberFound =
    any (==5) [1, 2, 5, 8, 9]

-- whether all elements in a list satisfy a condition
allBig =
    all (>4) [8, 9, 10, 2, 5]

{-
    iterate : start with a initial value, call a function on it to get
    result, and call again with the result as input, and so on.

    角谷猜想 again
-}
chain :: (Integral n) => n -> [n]
chain n =
    let
        jiaoGu :: (Integral n) => n -> n
        jiaoGu n =
            if even n then
                n `div` 2
            else
                3*n + 1
    in
        (takeWhile (/=1) $ iterate jiaoGu n) ++ [1]


{-
    Find key value pair in a list.
-}
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key list =
    foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing list
