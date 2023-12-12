module Day12 where

import Control.Monad (forM_, guard, join)
import Data.List (group, intercalate)
import Data.List.Split (splitOn, splitPlaces)

type Line = (String, [Int])

type Assignment = (String, [Int])

parseLine :: String -> Line
parseLine s = (springs, groups)
  where
    [springs, groupString] = words s
    groups = read $ "[" <> groupString <> "]"

unfoldLine :: Line -> Line
unfoldLine (springs, groups) = (intercalate "?" $ replicate 5 springs, join $ replicate 5 groups)

getPossibleArrangements :: Assignment -> [String]
getPossibleArrangements (springs, groups) = filter (matchesExactly groups) $ go ([""], springs)
  where
    go :: ([String], String) -> [String]
    go (options, []) = options
    go (options, '?' : xs) = go (filter (matches groups) (dots <> hashes), xs)
      where
        dots = map (<> ['.']) options
        hashes = map (<> ['#']) options
    go (options, x : xs) = go (filter (matches groups) $ map (<> [x]) options, xs)

matches :: [Int] -> String -> Bool
matches gs stringSoFar = case getHashGroups stringSoFar of
  [] -> True
  hashGroups ->
    let currentGroup : otherGroups = reverse $ zip hashGroups gs
     in uncurry (<=) currentGroup && all (uncurry (==)) otherGroups

matchesExactly :: [Int] -> String -> Bool
matchesExactly gs stringSoFar =
  let hashGroups = getHashGroups stringSoFar
   in length hashGroups == length gs
        && and (zipWith (==) gs (getHashGroups stringSoFar))

getHashGroups :: String -> [Int]
getHashGroups = map length . filter ((== '#') . head) . group

getPossibleAssignments :: Line -> [[Assignment]]
getPossibleAssignments (springs, gs) =
  filter (all isValidAssignment) $
    map (\p -> zip chunks $ p `splitPlaces` gs) ps
  where
    chunks = filter (not . null) $ splitOn "." $ springs
    ps = partitions (length gs) (length chunks)

isValidAssignment :: (String, [Int]) -> Bool
isValidAssignment (s, gs)
  | sum gs < length (filter (== '#') s) = False -- we already have too many #
  | all (== '#') s = length gs == 1 && head gs == length s
  | otherwise =
      let minCharLength = sum gs + (length gs - 1) -- we need at least 1 . in between
       in length s >= minCharLength

partitions :: Int -> Int -> [[Int]]
partitions x 1 = [[x]]
partitions x n = [i : rest | i <- [0 .. x - 1], rest <- partitions (x - i) (n - 1)]

countArrangements :: Line -> Int
countArrangements = sum . map (product . map (length . getPossibleArrangements)) . getPossibleAssignments

-- getPossibleArrangements :: Assignment -> [String]
--
main :: IO ()
main = do
  input <- map parseLine . lines <$> readFile "./12.txt"
  print $ sum $ countArrangements <$> input
  print $ sum $ countArrangements . unfoldLine <$> input
