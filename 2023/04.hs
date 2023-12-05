module Day04 where

import Control.Monad (forM_, liftM2)
import Data.Either (fromRight)
import Data.Vector qualified as V
import Text.Parsec

data Card = Card {cardNumber :: Int, winningNumbers :: [Int], myNumbers :: [Int]} deriving (Show)

cardParser :: Parsec String () Card
cardParser = do
  string "Card"
  skipMany space
  cardNumber <- read <$> many1 digit
  string ":"
  skipMany space
  winningNumbers <- many1 $ read <$> many1 digit <* many space
  string "|"
  skipMany space
  myNumbers <- many1 $ read <$> many1 digit <* many space
  return $ Card cardNumber winningNumbers myNumbers

getMatches :: Card -> [Int]
getMatches (Card _ winningNumbers myNumbers) = filter (`elem` winningNumbers) myNumbers

part1 :: [Card] -> Int
part1 = sum . map getPoints
  where
    getPoints c =
      let matches = length $ getMatches c
       in if matches == 0
            then 0
            else 2 ^ (matches - 1)

updateInventory :: V.Vector Int -> Card -> V.Vector Int
updateInventory inventory card = V.accum (+) inventory $ map (,youHave) cardsYouWin
  where
    matches = getMatches card
    cardNo = cardNumber card
    cardsYouWin = takeWhile (< length inventory) $ take (length matches) [cardNo ..]
    youHave = inventory V.! (cardNo - 1)

part2 :: [Card] -> Int
part2 cards = sum $ foldl updateInventory initialInventory cards
  where
    initialInventory = V.replicate (length cards) 0

main :: IO ()
main = do
  input <- lines <$> readFile "04.txt"
  let cards = map (fromRight (error "Parse error") . parse cardParser "") input
  print $ part1 cards
  print $ part2 cards
