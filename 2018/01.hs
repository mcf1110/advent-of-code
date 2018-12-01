module Day01 where
  import Data.List (scanl)

  parse :: String -> Int
  parse ('+':n) = read n
  parse n = read n

  freqTwice :: [Int] -> Int
  freqTwice a = findTwice [] freqs
    where freqs = scanl (+) 0 $ cycle a

  findTwice :: [Int] -> [Int] -> Int
  findTwice seen (x:rest)
    | x `elem` seen = x
    | otherwise = findTwice (x:seen) rest
  findTwice seen ([]) = head seen

  main = sum <$> (map parse) <$> lines <$> readFile "01.txt" >>= print


  main2 = freqTwice <$> (map parse) <$> lines <$> readFile "01.txt" >>= print
