module Day02 where

import Data.Either (fromRight)
import Text.Parsec

data Game = Game {gameId :: Int, cubeSets :: [[CubeDraw]]} deriving (Show)

data CubeDraw = CubeDraw {color :: Color, amount :: Int} deriving (Show)

data Color = Red | Green | Blue deriving (Show)

type RGB = (Int, Int, Int)

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green

doParse :: String -> Either ParseError Game
doParse = parse parseGame ""
  where
    parseGame = do
      string "Game "
      gameId <- read <$> many1 digit
      string ": "
      cubeSets <- sepBy1 parseCubeSet (string "; ")
      return $ Game gameId cubeSets
    parseCubeSet = sepBy1 parseCube (string ", ")
    parseCube = do
      amount <- read <$> many1 digit
      string " "
      color <- parseColor
      return $ CubeDraw color amount
    parseColor = do
      c <- string ("red" :: String) <|> string "green" <|> string "blue"
      return $ case c of
        "red" -> Red
        "green" -> Green
        "blue" -> Blue

part1 :: [Game] -> [Int]
part1 = map gameId . filter (all (isPossibleWith (12, 13, 14)) . cubeSets)

isPossibleWith :: RGB -> [CubeDraw] -> Bool
isPossibleWith (r, g, b) =
  all
    ( \(CubeDraw c a) -> case c of
        Red -> a <= r
        Green -> a <= g
        Blue -> a <= b
    )

part2 :: [Game] -> [Int]
part2 = map (getPower . getMaxCubes)
  where
    getPower (r, g, b) = r * g * b
    getMaxCubes :: Game -> RGB
    getMaxCubes (Game _ cubeSets) =
      foldl
        ( \(r, g, b) (CubeDraw c a) -> case c of
            Red -> (max a r, g, b)
            Green -> (r, max a g, b)
            Blue -> (r, g, max a b)
        )
        (0, 0, 0)
        $ concat cubeSets

main :: IO ()
main = do
  input <- map (fromRight (error "Parse error") . doParse) . lines <$> readFile "02.txt"
  print $ sum $ part1 input
  print $ sum $ part2 input