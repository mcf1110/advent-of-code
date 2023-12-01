module Day01 where

import Data.Char (isDigit)

part1 :: [String] -> [Int]
part1 = map ((read . (\ds -> [head ds, last ds])) . filter isDigit)

-- eighthree parses to 83
-- sevenine parses to 79
parseDigits :: String -> [Int]
parseDigits [] = []
parseDigits (x : xs) | isDigit x = read [x] : parseDigits xs
parseDigits xs@('o' : 'n' : 'e' : _) = 1 : parseDigits (tail xs)
parseDigits xs@('t' : 'w' : 'o' : _) = 2 : parseDigits (tail xs)
parseDigits xs@('t' : 'h' : 'r' : 'e' : 'e' : _) = 3 : parseDigits (tail xs)
parseDigits xs@('f' : 'o' : 'u' : 'r' : _) = 4 : parseDigits (tail xs)
parseDigits xs@('f' : 'i' : 'v' : 'e' : _) = 5 : parseDigits (tail xs)
parseDigits xs@('s' : 'i' : 'x' : _) = 6 : parseDigits (tail xs)
parseDigits xs@('s' : 'e' : 'v' : 'e' : 'n' : _) = 7 : parseDigits (tail xs)
parseDigits xs@('e' : 'i' : 'g' : 'h' : 't' : _) = 8 : parseDigits (tail xs)
parseDigits xs@('n' : 'i' : 'n' : 'e' : _) = 9 : parseDigits (tail xs)
parseDigits (_ : xs) = parseDigits xs

part2 :: [String] -> [Int]
part2 = map ((\ds -> (10 * head ds) + last ds) . parseDigits)

main :: IO ()
main = do
  input <- lines <$> readFile "01.txt"
  print $ sum $ part1 input
  print $ sum $ part2 input