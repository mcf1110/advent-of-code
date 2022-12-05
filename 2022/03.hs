module Day03 where

import Data.List (intersect, nub)
import Data.List.Split

divide :: String -> (String, String)
divide x = splitAt (length x `div` 2) x

priority :: Char -> Int
priority ch
  | ch >= 'a' = 1 + fromEnum ch - fromEnum 'a'
  | otherwise = 27 + fromEnum ch - fromEnum 'A'

main :: IO ()
main = do
  input <- lines <$> readFile "03.txt"
  print $ sum $ map priority $ nub . uncurry intersect . divide =<< input
  print $ sum $ map priority $ nub . foldl1 intersect =<< chunksOf 3 input