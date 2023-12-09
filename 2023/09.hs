module Day09 where

import Control.Monad (forM_)
import Data.List (nub)

derive :: [Int] -> [Int]
derive (x : y : xs) = (y - x) : derive (y : xs)
derive _ = []

extrapolate :: [Int] -> [Int]
extrapolate seq@(x : _)
  | all (== x) seq = repeat x
  | otherwise = scanl (+) x $ extrapolate $ derive seq

getNextInSeq :: [Int] -> Int
getNextInSeq seq = extrapolate seq !! length seq

main :: IO ()
main = do
  input <- lines <$> readFile "09.txt"
  let seqs :: [[Int]] = map (map read . words) input

  print $ sum $ map getNextInSeq seqs
  -- part2 was really easy lol
  print $ sum $ map (getNextInSeq . reverse) seqs
