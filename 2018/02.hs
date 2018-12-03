module Day02 where
  import qualified Data.Map as Map
  import Data.List

  myInput = lines <$> readFile "02.txt"

  count :: String -> Map.Map Char Int
  count = foldl (\m v -> Map.insertWith (+) v 1 m) Map.empty

  idToCount :: String -> (Int, Int)
  idToCount s =  (n 2, n 3)
    where
      m = Map.elems $ count s
      n x =  fromEnum $ not $ null $ filter (== x) m

  solvePart1 a = uncurry (*) $ foldl1 (\(x1, x2) (y1, y2) -> (x1+y1, x2+y2)) $ map idToCount a

  allPossible :: Eq a => [a] -> [(a, a)]
  allPossible ls = [(x,y) | x <- ls, y <- ls, x /= y]

  diffSize :: Eq a => [a] -> [a] -> Int
  diffSize a b = sum $ map fromEnum $ zipWith (/=) a b

  solvePart2 ls = head $ map ((uncurry intersect) . fst) $
      filter ((==1). snd) $
      zip comb $
      map (uncurry diffSize) comb
    where comb = allPossible ls

  part1 = solvePart1 <$> myInput >>= print
  part2 = solvePart2 <$> myInput >>= print
