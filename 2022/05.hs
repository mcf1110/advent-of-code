module Day05 where

import Data.Char (isNumber, isSpace)
import Data.List (transpose)
import Data.List.Split
import Data.Maybe (mapMaybe)

type Move = (Int, Int, Int)

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

parseCrate :: String -> Maybe String
parseCrate (i : as) | isNumber i = Just $ reverse $ filter (not . isSpace) as
parseCrate _ = Nothing

parseMove :: String -> Move
parseMove str = (howMany, from - 1, to - 1)
  where
    [howMany, from, to] = map read $ filter (all isNumber) $ words str

applyMovePart1 :: [String] -> Move -> [String]
applyMovePart1 stacks (0, from, to) = stacks
applyMovePart1 stacks (howMany, from, to) = applyMovePart1 (at to (h :) $ at from tail stacks) (howMany - 1, from, to)
  where
    h = head $ stacks !! from

applyMovePart2 :: [String] -> Move -> [String]
applyMovePart2 stacks (howMany, from, to) = at to (h <>) $ at from (drop howMany) stacks
  where
    h = take howMany $ stacks !! from

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list <> [newVal] <> drop (pos + 1) list

at :: Int -> (a -> a) -> [a] -> [a]
at pos f list = replace pos (f $ list !! pos) list

main :: IO ()
main = do
  [stateStr, instStr] <- splitOn "\n\n" <$> readFile "05.txt"
  let stacks = mapMaybe parseCrate (rotateRight $ lines stateStr)
      moves = parseMove <$> lines instStr

  print $ map head $ foldl applyMovePart1 stacks moves
  print $ map head $ foldl applyMovePart2 stacks moves