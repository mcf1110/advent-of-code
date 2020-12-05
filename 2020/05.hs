module Day05 where
import Data.List

parse :: String -> (Int, Int)
parse str = (s vert, s hor)
    where
        vert = map (== 'B') $ reverse $ take 7 str
        hor = map (== 'R') $ reverse $ drop 7 str
        twos = map (2^) [0..]
        s x = sum $ zipWith (\p v -> if v then p else 0) twos x
sid (a, b) = 8*a + b
allIds = map sid $ [(x,y) | x <- [0..127], y <- [0..7]]
main = do
    inp <- lines <$> readFile "05.txt"
    let ids = sid . parse <$> inp
        hi = maximum ids
        lo = minimum ids
        missingIds = allIds \\ ids
    print $ hi
    print $ head $ filter (\x -> x > lo && x < hi) missingIds