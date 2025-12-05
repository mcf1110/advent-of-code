
module Day03 where
import Data.List
import Data.Ord

type Joltage = Integer
type Bank = [Joltage]


parse :: String -> [Bank]
parse = map (map $ read . (:[])) . lines

partWith :: Int -> [Bank] -> Joltage
partWith d = sum . map (toInt . findMax d)

toInt :: [Joltage] -> Joltage
toInt = foldl1' (\acc x -> 10*acc + x)

findMax :: Ord a => Int -> [a] -> [a]
findMax 0 _ = []
findMax d js = toList $ maximumBy (comparing fst) $ reverse $ takeWhile ((>= (d-1)).length.snd) $ zip js $ tail $ tails js
    where
        toList (a, bs) = a:(findMax (d-1) bs)

part1, part2 :: [Bank] -> Joltage
part1 = partWith 2
part2 = partWith 12

main :: IO ()
main = do
  x <- parse <$> readFile "03.txt"
  print $ part1 x
  print $ part2 x