{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid reverse" #-}
module Day07 where

import Data.List (group, partition, sort, sortOn)

data CardRank
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Show)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Enum, Show)

parseRankWithoutJoker :: Char -> CardRank
parseRankWithoutJoker '2' = Two
parseRankWithoutJoker '3' = Three
parseRankWithoutJoker '4' = Four
parseRankWithoutJoker '5' = Five
parseRankWithoutJoker '6' = Six
parseRankWithoutJoker '7' = Seven
parseRankWithoutJoker '8' = Eight
parseRankWithoutJoker '9' = Nine
parseRankWithoutJoker 'T' = Ten
parseRankWithoutJoker 'J' = Jack
parseRankWithoutJoker 'Q' = Queen
parseRankWithoutJoker 'K' = King
parseRankWithoutJoker 'A' = Ace
parseRankWithoutJoker _ = error "Invalid rank"

parseRankWithJoker :: Char -> CardRank
parseRankWithJoker 'J' = Joker
parseRankWithJoker c = parseRankWithoutJoker c

data Hand = Hand
  { handType :: HandType,
    handRanks :: [CardRank]
  }
  deriving (Eq, Ord, Show)

parseHand :: (Char -> CardRank) -> String -> Hand
parseHand parseRank s = Hand handType handRanks
  where
    handRanks = map parseRank $ take 5 s
    handType = case reverse $ sort $ map length $ moveJokers $ group $ sort handRanks of
      [1, 1, 1, 1, 1] -> HighCard
      [2, 1, 1, 1] -> OnePair
      [2, 2, 1] -> TwoPair
      [3, 1, 1] -> ThreeOfAKind
      [3, 2] -> FullHouse
      [4, 1] -> FourOfAKind
      [5] -> FiveOfAKind
      x -> error $ "Invalid hand: " <> show (moveJokers $ group $ sort handRanks, group $ sort $ handRanks)

data HandAndBid = HandAndBid
  { hand :: Hand,
    bid :: Int
  }
  deriving (Eq, Ord, Show)

parseHandAndBid :: (Char -> CardRank) -> String -> HandAndBid
parseHandAndBid parseRank s = HandAndBid (parseHand parseRank hand) (read bid)
  where
    [hand, bid] = words s

moveJokers :: [[CardRank]] -> [[CardRank]]
moveJokers rs | all (Joker `notElem`) rs = rs -- no jokers, no problem
moveJokers rs | length rs == 1 && Joker `elem` head rs = rs -- all jokers, no problem
moveJokers rs = newLargest : drop 1 sorted
  where
    (jokers, nonJokers) = partition ((== Joker) . head) rs
    jokerCount = length $ head jokers
    -- add jokerCount cards to the largest group
    sorted = reverse $ sortOn length nonJokers
    newLargest = head sorted ++ replicate jokerCount Joker

main :: IO ()
main = do
  input <- lines <$> readFile "./07.txt"
  let part1 = map (parseHandAndBid parseRankWithoutJoker) input
      part2 = map (parseHandAndBid parseRankWithJoker) input
      run x = sum $ zipWith (*) (map bid $ sort x) [1 ..]
  print $ run part1
  print $ run part2