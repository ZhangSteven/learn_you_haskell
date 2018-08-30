{-
    A knight's quest

    From "Learn You a Haskell", chapter on Monads.

    A problem on positions a knight can reach on a chess board.
-}
import Control.Monad (guard)


type KnightPos = (Int, Int)
type KnightPosHistory = [KnightPos]


-- positions a knight can reach within one step of move
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [ (c+2, r+1)
                , (c+2, r-1)
                , (c+1, r+2)
                , (c+1, r-2)
                , (c-2, r+1)
                , (c-2, r-1)
                , (c-1, r+2)
                , (c-1, r-2) ]
    guard (c' > 0 && c' < 9 && r' > 0 && r' < 9)
    return (c', r')


-- what positions can we reach in 3 steps
in3 :: KnightPos -> [KnightPos]
in3 start =
    return start >>= moveKnight >>= moveKnight >>= moveKnight


-- can we reach position end from position start in exactly 3 steps?
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end =
    end `elem` in3 start


{-
    Instead of just keeping the positions reached, now we work on the
    list of moves taken.
-}
moveKnightHistory :: KnightPosHistory -> [KnightPosHistory]
moveKnightHistory ((c, r) : xs) = do
    (c', r') <- moveKnight (c, r)
    return ((c', r') : (c, r) : xs)


in3History :: KnightPos -> [KnightPosHistory]
in3History start =
    [[start]] >>= moveKnightHistory >>= moveKnightHistory >>= moveKnightHistory


-- Tell you the path from start position to end position
pathIn3 :: KnightPos -> KnightPos -> [KnightPosHistory]
pathIn3 start end =
    map reverse $ filter (reached end) $ in3History start where
        reached :: KnightPos -> KnightPosHistory -> Bool
        reached pos history =
            pos == head history
