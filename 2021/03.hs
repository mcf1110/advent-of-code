module Day03 where

import Control.Monad (liftM2)
import Data.Bool (bool)
import Data.List (group, sort, sortOn, transpose)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))

type Binary = [Bool]

main :: IO ()
main = do
  input <- fmap (map (== '1')) . lines <$> readFile "03.txt"
  print $ p1 input
  print $ p2 input

p1 :: [Binary] -> Int
p1 = liftM2 (*) toInt (toInt . map not) . map mode . transpose

p2 :: [[Bool]] -> Int
p2 = liftM2 (*) (goWith mode) (goWith least) . (zip =<< (toInt <$>))

goWith :: Eq a => ([a] -> a) -> [(b, [a])] -> b
goWith f xs = if length filtered == 1 then fst (head filtered) else goWith f nextBit
  where
    m = f $ head $ transpose $ snd <$> xs
    filtered = filter (\t -> head (snd t) == m) xs
    nextBit = map (tail <$>) filtered

toInt :: [Bool] -> Int
toInt = sum . zipWith (*) ((2 ^) <$> [0 ..]) . map (bool 0 1) . reverse

mode :: Binary -> Bool
mode = head . head . sortOn (Down . length) . group . sortOn Down

least :: Binary -> Bool
least = not . mode