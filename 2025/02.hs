
module Day02 where

import Data.List.Split
import Data.List

type Range = (Int, Int)

parse :: String -> [Range]
parse = map ((\[x,y] -> (read x, read y)).splitOn "-") . splitOn ","

part1 :: [Range] -> Int
part1 = sum . filter isDupe . concatMap (uncurry enumFromTo)
    where
        isDupe number = let 
            strNumber = show number
            l = length strNumber
            (n1, n2) = splitAt (l `div` 2) strNumber
             in even l && n1 == n2


part2 :: [Range] -> Int
part2 = sum . filter isDupe . concatMap (uncurry enumFromTo)
    where
        divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]
        isDupe number = let 
            strNumber = show number
            l = length strNumber
            ds = divisors l
            isDupeWith d = (==1) $ length $ nub $ chunksOf d strNumber
             in any isDupeWith ds

main :: IO ()
main = do
  x <- parse <$> readFile "02.txt"
  print $ part1 x
  print $ part2 x