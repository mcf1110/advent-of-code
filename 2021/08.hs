module Day08 where

import Control.Monad
import qualified Data.IntMap as M
import Data.List (permutations, (\\))
import Data.List.Split
import qualified Data.Set as S

display :: Int -> String
display 0 = "abcefg"
display 1 = "cf"
display 2 = "acdeg"
display 3 = "acdfg"
display 4 = "bcdf"
display 5 = "abdfg"
display 6 = "abdefg"
display 7 = "acf"
display 8 = "abcdefg"
display 9 = "abcdfg"

strToBits :: String -> [Bool]
strToBits str = [c `elem` str | c <- ['a' .. 'g']]

main :: IO ()
main = do
  input <- fmap parse . lines <$> readFile "08.txt"
  print $ part1 input
  print $ part2 input

parse :: String -> ([S.Set Char], [String])
parse st = let [allDigits, shownDigits] = splitOn " | " st in (S.fromList <$> words allDigits, words shownDigits)

part1 :: [([S.Set Char], [String])] -> Int
part1 = sum . foldl acc (M.fromList [(k, 0) | k <- [2, 3, 4, 7]])
  where
    acc :: M.IntMap Int -> ([S.Set Char], [String]) -> M.IntMap Int
    acc mp (_, shown) = foldl (\m s -> M.adjust succ (length s) m) mp shown

part2 :: [([S.Set Char], [String])] -> Int
part2 = sum . fmap solve
  where
    solve :: ([S.Set Char], [String]) -> Int
    solve (digits, values) = sum $ zipWith (*) [1000, 100, 10, 1] $ map replace values
      where
        mapping = M.toList $ findMapping digits
        replace :: String -> Int
        replace v = fst $ head $ filter ((== S.fromList v) . snd) mapping

type Possibility = String

type Mapping = M.IntMap (S.Set Char)

testHypothesis :: Int -> S.Set Char -> [Possibility] -> [Possibility]
testHypothesis n set = filter ((== set) . mask n)
  where
    mask :: Int -> String -> S.Set Char
    mask n str = S.fromList $ map snd $ filter fst $ zip (strToBits $ display n) str

testNumber :: [S.Set Char] -> Int -> Mapping -> [Possibility] -> [(Mapping, [Possibility])]
testNumber digits n currentMapping ps = map test fittingDigits
  where
    fittingDigits = filter ((== (length . display) n) . S.size) $ digits \\ M.elems currentMapping
    test set = (M.insert n set currentMapping, testHypothesis n set ps)

findMapping :: [S.Set Char] -> Mapping
findMapping digits = fst $ head $ filter (not . null . snd) $ foldM (\(map, ps) n -> testNumber digits n map ps) (M.empty, allPossibilities) orderToTry
  where
    orderToTry = [1, 7, 4, 2, 3, 5, 0, 6, 9, 8]
    allPossibilities = permutations ['a' .. 'g']