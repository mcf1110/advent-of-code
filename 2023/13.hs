{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

import Control.Applicative ((<|>))
import Data.List (find, intersect, transpose)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T

findReflections :: String -> [Int]
findReflections = map (length . fst) . filter (and . uncurry (zipWith (==))) . zipIt

zipIt :: String -> [(String, String)]
zipIt (x : xs) = go [x] xs
  where
    go :: String -> String -> [(String, String)]
    go _ [] = []
    go acc l = (acc, l) : go (head l : acc) (tail l)

findCommonReflections :: [String] -> [Int]
findCommonReflections = foldl1 intersect . map findReflections

part1 :: [String] -> Int
part1 matrix = cols + rows
  where
    cols = sum $ findCommonReflections matrix
    rows = sum $ map (* 100) $ findCommonReflections $ transpose matrix

fixSmudge :: [String] -> Maybe Int
fixSmudge str = fmap fst $ find ((== (length str - 1)) . snd) $ M.toList counts
  where
    reflections = map findReflections str
    counts = M.unionsWith (+) $ map (M.fromAscList . map (,1)) reflections

part2 :: [String] -> Int
part2 str = fromJust $ fixSmudge str <|> ((100 *) <$> fixSmudge (transpose str))

main :: IO ()
main = do
  input <- map (lines . T.unpack) . T.splitOn "\n\n" . T.pack <$> readFile "./13.txt"
  print $ sum $ part1 <$> input
  print $ sum $ part2 <$> input