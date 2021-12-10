{-#LANGUAGE TupleSections #-}
module Day09 where

import Data.List (sort)
import qualified Data.Matrix as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S

type Matrix = M.Matrix Int

type Pos = (Int, Int)

type Basin = S.Set Pos

main :: IO ()
main = do
  input <- parse <$> readFile "09.txt"
  print $ part1 input
  print $ part2 input


parse :: String -> Matrix
parse = M.fromLists . map (map (read . (: []))) . lines

part1 = sum . map ((+ 1) . snd) . getAllLows

part2 m = product $ take 3 $ reverse $ sort (S.size . expandBasinAt m . fst <$> getAllLows m)

expandBasinAt :: Matrix -> Pos -> S.Set Pos
expandBasinAt m pos = go (S.singleton pos) pos
  where
    go set p = foldl go newSet toCheck
      where
        toCheck = map fst $ filter (\(cp, cv) -> cv < 9 && S.notMember cp set) $ getNeighbors m p
        newSet = set <> S.fromList toCheck

getAllLows :: Matrix -> [(Pos, Int)]
getAllLows m = catMaybes [((r, c),) <$> checkLow m (r, c) | r <- [1 .. (M.nrows m)], c <- [1 .. (M.ncols m)]]

getNeighbors :: Matrix -> Pos -> [(Pos, Int)]
getNeighbors m (r, c) = mapMaybe (\(i, j) -> ((i, j),) <$> M.safeGet i j m) [(r + 1, c), (r -1, c), (r, c + 1), (r, c -1)]

checkLow :: Matrix -> Pos -> Maybe Int
checkLow m (r, c)
  | all (v <) nValues = Just v
  | otherwise = Nothing
  where
    v = m M.! (r, c)
    nValues = snd <$> getNeighbors m (r, c)
