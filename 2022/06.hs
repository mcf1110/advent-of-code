module Day06 where

import Control.Comonad (duplicate)
import Data.List
import Data.List.NonEmpty qualified as N

allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = x `notElem` xs && allDifferent xs

findNDiffChars :: Int -> String -> Maybe (Int, String)
findNDiffChars n input = find (allDifferent . snd) $ zip [n ..] $ map (N.take n) $ N.toList $ duplicate $ N.fromList input

main :: IO ()
main = do
  input <- readFile "06.txt"
  print $ findNDiffChars 4 input
  print $ findNDiffChars 14 input