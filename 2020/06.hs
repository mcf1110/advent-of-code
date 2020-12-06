module Day06 where
import Data.List.Split
import Data.List

main = do
    inp <- (map words) <$> splitOn "\n\n" <$> readFile "06.txt"
    print $ sum $ (length . nub . concat) <$> inp
    print $ sum $ (length . foldl1 intersect) <$> inp