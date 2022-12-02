module Day02 where

import Data.List
import Data.Maybe (fromJust)

data RPS = Rock | Paper | Scissors deriving (Enum, Show, Eq)

data Outcome = Lose | Tie | Win deriving (Enum, Show, Eq)

parseFrom :: Enum e => Char -> Char -> e
parseFrom initialChar char = toEnum $ fromEnum char - fromEnum initialChar

-- | Left is who you want to win
getOutcome :: (RPS, RPS) -> Outcome
getOutcome (Paper, Rock) = Win
getOutcome (Scissors, Paper) = Win
getOutcome (Rock, Scissors) = Win
getOutcome (x, y)
  | x == y = Tie
  | otherwise = Lose

toGetOutcome :: Outcome -> RPS -> RPS
toGetOutcome outcome enemy = fromJust $ find (\x -> getOutcome (x, enemy) == outcome) [Rock, Paper, Scissors]

problem1 :: [String] -> Int
problem1 = sum . map (score . parse)
  where
    parse [op, ' ', you] = (parseFrom 'A' op, parseFrom 'X' you)
    score (op, you) = (3 * fromEnum (getOutcome (you, op))) + (fromEnum you + 1)

problem2 :: [String] -> Int
problem2 = sum . map (score . parse)
  where
    parse [op, ' ', desiredOutcome] = (parseFrom 'A' op, parseFrom 'X' desiredOutcome)
    score (op, desiredOutcome) = 3 * fromEnum desiredOutcome + fromEnum (toGetOutcome desiredOutcome op) + 1

main :: IO ()
main = do
  input <- lines <$> readFile "02.txt"
  print $ problem1 input
  print $ problem2 input