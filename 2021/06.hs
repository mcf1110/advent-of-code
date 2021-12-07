module Day06 where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad (forM_)

main :: IO ()
main = do
  input <- readFile "06.txt"
  let vec = toVector $ (read $ "["<>input<>"]" :: [Int])
  print $ sum $ (iterate step vec) !! 80
  print $ sum $ (iterate step vec) !! 256

toVector :: [Int] -> V.Vector Int
toVector xs = go (V.replicate 9 0) xs
    where 
        go v [] = v
        go v (x:xs) = go (V.modify (\vec -> MV.modify vec succ x) v) xs
         

step :: V.Vector Int -> V.Vector Int
step v = V.modify action v
    where 
        parents = V.head v
        action vec = do
            forM_ [0..7] (\i -> MV.swap vec i (i+1)) -- bubbles up 0-th element to the end
            MV.modify vec (+parents) 6