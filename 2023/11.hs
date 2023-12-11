module Day11 where

import Control.Monad (guard)
import Data.List (transpose)

getGalaxyCoords :: [[Char]] -> [(Int, Int)]
getGalaxyCoords rows = do
  (i, row) <- zip [0 ..] rows
  (j, col) <- zip [0 ..] row
  guard $ col == '#'
  return (i, j)

getEmptyRows :: [[Char]] -> [Int]
getEmptyRows rows = do
  (i, row) <- zip [0 ..] rows
  guard $ all (== '.') row
  return i

-- expandSpaceBy :: [String] -> Int -> [(Int, Int)] -> [(Int, Int)]
expandSpaceBy rows n = map expand
  where
    emptyRows = getEmptyRows rows
    emptyCols = getEmptyRows $ transpose rows
    expand (i, j) = (i + ie, j + je)
      where
        ie = (n - 1) * length (takeWhile (< i) emptyRows)
        je = (n - 1) * length (takeWhile (< j) emptyCols)

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x : xs) = map (x,) xs <> pairwise xs

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  input <- lines <$> readFile "11.txt"
  let runWith n = sum $ map (uncurry manhattanDistance) $ pairwise $ expandSpaceBy input n $ getGalaxyCoords input
  print $ runWith 2
  print $ runWith 1_000_000
