module Day14 where

import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Matrix qualified as M

data Direction = N | W | S | E deriving (Show, Eq)

tiltAll :: Direction -> M.Matrix Char -> M.Matrix Char
tiltAll d m = foldl (tiltSingleRock d) m $ getOrder d m

getOrder :: Direction -> M.Matrix Char -> [(Int, Int)]
getOrder d m = case d of
  N -> [(i, j) | i <- [1 .. M.nrows m], j <- [1 .. M.ncols m]]
  W -> [(i, j) | j <- [1 .. M.ncols m], i <- [1 .. M.nrows m]]
  S -> [(i, j) | i <- [M.nrows m, M.nrows m - 1 .. 1], j <- [1 .. M.ncols m]]
  E -> [(i, j) | j <- [M.ncols m, M.ncols m - 1 .. 1], i <- [1 .. M.nrows m]]

tiltSingleRock :: Direction -> M.Matrix Char -> (Int, Int) -> M.Matrix Char
tiltSingleRock d m (i, j)
  | M.unsafeGet i j m /= 'O' = m
  | otherwise = case freeRollPath of
      [] -> m
      path -> M.unsafeSet 'O' (fst $ last path) $ M.unsafeSet '.' (i, j) m
  where
    rollPath = map (\(i, j) -> ((i, j), M.getElem i j m)) $ getPath m d (i, j)
    freeRollPath = takeWhile ((== '.') . snd) rollPath

getPath :: M.Matrix Char -> Direction -> (Int, Int) -> [(Int, Int)]
getPath _ N (1, _) = []
getPath m N (i, j) = (i - 1, j) : getPath m N (i - 1, j)
-- South
getPath m S (i, j)
  | i == M.nrows m = []
  | otherwise = (i + 1, j) : getPath m S (i + 1, j)
-- West
getPath _ W (_, 1) = []
getPath m W (i, j) = (i, j - 1) : getPath m W (i, j - 1)
-- East
getPath m E (i, j)
  | j == M.ncols m = []
  | otherwise = (i, j + 1) : getPath m E (i, j + 1)

runCycle :: M.Matrix Char -> M.Matrix Char
runCycle m = foldl (flip tiltAll) m [N, W, S, E]

findRepeatingPattern :: [M.Matrix Char] -> (Int, Int) -- start, length
findRepeatingPattern ms = go 0 ms Map.empty
  where
    go i (m : ms) seen =
      case Map.lookup (M.toLists m) seen of
        Just j -> (j, i - j)
        Nothing -> go (i + 1) ms (Map.insert (M.toLists m) i seen)

totalLoad :: M.Matrix Char -> Int
totalLoad m = sum $ zipWith (*) [1 ..] $ reverse $ map rowLoad lists
  where
    lists = M.toLists m
    rowLoad row = length $ filter (== 'O') row

cycleAt :: Int -> [M.Matrix Char] -> M.Matrix Char
cycleAt i c = if i < start then c !! i else c !! (start + (i - start) `mod` len)
  where
    (start, len) = findRepeatingPattern c

main :: IO ()
main =
  do
    input <- M.fromLists . lines <$> readFile "14.txt"
    print $ totalLoad $ tiltAll N input
    -- for this to run, you can't use ghci
    -- compile with ghc and run it (takes a couple seconds, but it works)
    print $ totalLoad $ cycleAt 1000000000 $ iterate runCycle input