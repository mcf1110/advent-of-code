module Day01 where
  import Data.List (scanl)

  parse :: String -> Int
  parse ('+':n) = read n
  parse n = read n

  freqTwice :: [Int] -> Int
  freqTwice a = findTwice [] $ scanl (+) 0 $ cycle a

  findTwice :: [Int] -> [Int] -> Int
  findTwice seen (x:rest)
    | x `elem` seen = x
    | otherwise = findTwice (x:seen) rest

  myInput = (map parse) <$> lines <$> readFile "01.txt"
  part1 = myInput >>= print . sum
  part2 = myInput >>= print . freqTwice
