module Day01 where

import Control.Monad (ap)

p1 :: [Integer] -> Int
p1 = length . filter (uncurry (<)) . ap zip tail

p2 :: [Integer] -> Int
p2 x = p1 $ zipWith3 (\a b c -> a + b + c) x (tail x) (tail $ tail x)

main :: IO ()
main = do
  input <- fmap read . lines <$> readFile "01.txt"
  print $ p1 input
  print $ p2 input