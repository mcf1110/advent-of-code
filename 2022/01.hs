module Day01 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- map (sum . map read) . splitWhen null . lines <$> readFile "01.txt"
  print $ maximum input
  print $ sum $ take 3 $ reverse $ sort input