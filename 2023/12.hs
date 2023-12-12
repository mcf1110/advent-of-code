module Day12 where

import Control.Monad (forM_, guard, join)
import Data.List (group, intercalate)
import Data.List.Split (splitOn, splitPlaces)
import Debug.Trace
import qualified Data.Map as M

type Line = (String, [Int])

type Assignment = (String, [Int])

parseLine :: String -> Line
parseLine s = (springs, groups)
  where
    [springs, groupString] = words s
    groups = read $ "[" <> groupString <> "]"

unfoldLine :: Line -> Line
unfoldLine (springs, groups) = (intercalate "?" $ replicate 5 springs, join $ replicate 5 groups)

countArrangements :: Line -> Int
countArrangements i = fst $ countArrangements' i M.empty
  where 
  -- memoized version of countArrangements
  countArrangements' :: Line -> M.Map Line Int -> (Int, M.Map Line Int)
  countArrangements' k m | k `M.member` m = (m M.! k, m)
  countArrangements' ("", []) m = (1, m) -- if no springs left, we must also have no groups left. Then, there is only one arrangement
  countArrangements' ("", groups) m = (0, m) -- otherwise, if we have no springs left but still groups, we have no arrangement
  -- if we have no groups left but still springs, none of them must be hashtags
  countArrangements' (springs, []) m = let result = if '#' `elem` springs then 0 else 1 in 
    (result, M.insert (springs, []) result m)
    -- if the sum of the groups is larger than the remaining springs, we have no arrangement
  countArrangements' (springs, groups) m | sum (groups) > length (springs) = 
    (0, M.insert (springs, groups) 0 m)
  countArrangements' (springs@(s : ss), groups@(g : gs)) m = case s of
    '?' -> let (a, m') = asDot m
               (b, m'') = asHash m'
            in (a + b, M.insert (springs, groups) (a + b) m'')
    '#' -> let (a, m') = asHash m
            in (a, M.insert (springs, groups) (a) m')
    '.' -> let (a, m') = asDot m
            in (a, M.insert (springs, groups) (a) m')
    where
      asDot = countArrangements' (ss, groups) -- if we use the current spring as a dot
      asHash m
        | '.' `elem` take g springs = (0, m) -- if there is a dot in the next g springs, this can't the start of this group
        | g /= length (springs) && springs !! g == '#' = (0, m)
        | otherwise = countArrangements' (drop g ss, gs) m

main :: IO ()
main = do
  input <- map parseLine . lines <$> readFile "./12.txt"
  print $ sum $ countArrangements <$> input
  print $ sum $ countArrangements . unfoldLine <$> input