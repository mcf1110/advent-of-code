module Day07 where
  import IntCode
  import Data.List(permutations)

  runAmps :: String -> [Int] -> Int
  runAmps pg modes = go modes [0]
    where
      go [] (input:_) = input
      go (m:ms) (input:_) = go ms $ run pg [m, input]
  
  findMax :: String -> Int
  findMax pg = maximum $ fmap (runAmps pg) $ permutations $ [0..4]

  main = print =<< findMax <$> readFile "7.txt"
