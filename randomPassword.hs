{-
    Randomness in Haskell

    Instead of feeding an integer to the mkStdGen function, we can also use
    the getStdGen :: IO StdGen function to get a StdGen instance when the
    program starts. This way, we can have a different random generator each
    time the program runs.
-}
import System.Random (StdGen, getStdGen, randomRs)

main = do
    gen <- getStdGen
    putStrLn $ genPassword gen


{-
    Generate a 12 character random password consisting of upper and lower
    cases letters.
-}
genPassword :: StdGen -> String
genPassword gen =
    let
        letters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        indices = take 12 $ randomRs (0, length letters - 1) gen
    in
        map (\index -> letters !! index) indices
