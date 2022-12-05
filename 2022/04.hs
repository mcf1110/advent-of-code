module Day04 where

import Data.List.Split

type Elf = (Int, Int)

parseLine :: String -> (Elf, Elf)
parseLine s = ((e1s, e1e), (e2s, e2e))
  where
    [s1, s2] = splitOn "," s
    [e1s, e1e] = read <$> splitOn "-" s1
    [e2s, e2e] = read <$> splitOn "-" s2

fullyContains :: Elf -> Elf -> Bool
fullyContains (s1, e1) (s2, e2)
  | s1 == s2 || e1 == e2 = True
  | s1 > s2 = fullyContains (s2, e2) (s1, e1)
  | otherwise = e2 < e1

overlaps :: Elf -> Elf -> Bool
overlaps elf1@(s1, e1) elf2@(s2, e2)
  | s1 == s2 = True
  | s1 > s2 = overlaps elf2 elf1
  | s2 <= e1 = True
  | otherwise = False

main :: IO ()
main = do
  input <- lines <$> readFile "04.txt"
  print $ length $ filter (uncurry fullyContains) $ parseLine <$> input
  print $ length $ filter (uncurry overlaps) $ parseLine <$> input
