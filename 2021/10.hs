module Day10 where

import Data.List (sort)
import Data.Maybe (mapMaybe)

data InputType = Valid | Incomplete String | Corrupted {expected :: Char, found :: Char} deriving (Show)

main :: IO ()
main = do
  input <- fmap consumeInput . lines <$> readFile "10.txt"
  print $ sum $ mapMaybe getCorruptedPoints $ input
  print $ median $ mapMaybe getIncompletePoints $ input

close :: Char -> Char
close '(' = ')'
close '{' = '}'
close '[' = ']'
close '<' = '>'
close _ = undefined

consumeInput :: String -> InputType
consumeInput = go []
  where
    go [] [] = Valid
    go stack [] = Incomplete stack
    go stack (x : xs)
      | x `elem` "({[<" = go (close x : stack) xs
    go (s : stack) (x : xs) = if x == s then go stack xs else Corrupted s x
    go [] (x : xs) = Corrupted '?' x -- we got a closing when there was nothing to close

getCorruptedPoints :: InputType -> Maybe Int
getCorruptedPoints (Corrupted e f) = case f of
  ')' -> Just 3
  ']' -> Just 57
  '}' -> Just 1197
  '>' -> Just 25137
  _ -> Nothing
getCorruptedPoints _ = Nothing

getIncompletePoints :: InputType -> Maybe Int
getIncompletePoints (Incomplete e) = Just $ foldl (\acc ch -> acc * 5 + score ch) 0 e
  where
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4
    score _ = undefined
getIncompletePoints _ = Nothing

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)