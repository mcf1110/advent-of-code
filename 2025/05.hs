
module Day05 where

import qualified Data.IntervalSet as IS
import Data.List.Split
import Data.Interval ((<=..<=), Interval(..), width)

-- https://hackage-content.haskell.org/package/data-interval-2.1.2/docs/Data-IntervalSet.html

type Bound = IS.Extended Integer
type Range = Interval Integer
type Ranges = IS.IntervalSet Integer

parse :: String -> (Ranges, [Integer])
parse xs = (intervalSet, ids)
    where 
        (intervalStrs, idStrs) = fmap tail $ span (/="") $ lines xs
        ids = map (read @Integer) idStrs
        intervalSet = IS.fromList $ map makeRange intervalStrs

makeRange :: String -> Range
makeRange str = l <=..<= u
  where [l,u] = map (IS.Finite . (read @Integer)) $ splitOn "-" str

part1 :: [Integer] -> Ranges -> Int
part1 ids ranges = length $ filter (`IS.member` ranges) ids


part2 :: Ranges -> Integer
part2 = sum . map ((+1).width) . IS.toList

main :: IO ()
main = do
  (ranges, ids) <- parse <$> readFile "05.txt"
  print $ part1 ids ranges
  print $ part2 ranges