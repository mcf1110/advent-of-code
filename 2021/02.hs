module Day02 where

import Data.Char (toUpper)

main :: IO ()
main = do
  input <- fmap parse . lines <$> readFile "02.txt"
  print $ applyRule rule1 input
  print $ applyRule rule2 input

data Direction = U | D | F deriving (Read, Show)

type State = (Integer, Integer, Integer)

type Step = (Direction, Integer)

type Rule = State -> Step -> State

applyRule :: Rule -> [Step] -> Integer
applyRule rule steps = let (x, y, _) = foldl rule (0, 0, 0) steps in x * y

rule1 :: Rule
rule1 (x, y, a) (F, amt) = (x + amt, y, a)
rule1 (x, y, a) (D, amt) = (x, y + amt, a)
rule1 (x, y, a) (U, amt) = (x, y - amt, a)

rule2 :: Rule
rule2 (x, y, a) (F, amt) = (x + amt, y + amt * a, a)
rule2 (x, y, a) (D, amt) = (x, y, a + amt)
rule2 (x, y, a) (U, amt) = (x, y, a - amt)

parse :: String -> Step
parse str = let [dir, amt] = words str in (read . (: []) . toUpper . head $ dir, read amt)
