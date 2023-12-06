module Day05 where

import Data.Foldable (find)
import Data.List.Split (chunksOf, splitOn)

data Entry = Entry
  { sourceStart :: Int,
    destinationStart :: Int,
    rangeLength :: Int
  }
  deriving (Show, Eq)

type Mapping = [Entry]

data Range = Range
  { start :: Int,
    end :: Int
  }
  deriving (Show, Eq)

sourceEnd :: Entry -> Int
sourceEnd e = sourceStart e + rangeLength e - 1

parseSeedInts :: String -> [Int]
parseSeedInts = map read . tail . words

parseMappings :: [String] -> [Mapping]
parseMappings = map (map parseEntry . tail . lines)

parseEntry :: String -> Entry
parseEntry str = Entry sourceStart destinationStart rangeLength
  where
    [destinationStart, sourceStart, rangeLength] = read <$> words str

intsToSeeds :: [Int] -> [Range]
intsToSeeds = map (\a -> Range a a)

intsToRanges :: [Int] -> [Range]
intsToRanges = map (\[a, b] -> Range a (a + b - 1)) . chunksOf 2

applyMappingToRange :: Mapping -> Range -> [Range]
applyMappingToRange es r = map applyIt $ foldl trySplitRange [(r, Nothing)] es
  where
    applyIt :: (Range, Maybe Entry) -> Range
    applyIt (r, Nothing) = r
    applyIt (r, Just e) =
      Range
        (destinationStart e + start r - sourceStart e)
        (destinationStart e + end r - sourceStart e)
    trySplitRange :: [(Range, Maybe Entry)] -> Entry -> [(Range, Maybe Entry)]
    trySplitRange rmes e =
      concatMap
        ( \(r, me) -> case me of
            Nothing -> tryMatchEntry r e
            Just _ -> [(r, me)]
        )
        rmes

tryMatchEntry :: Range -> Entry -> [(Range, Maybe Entry)]
tryMatchEntry range entry
  | sourceEnd entry < start range = [(range, Nothing)]
  | end range < sourceStart entry = [(range, Nothing)]
  -- otherwise there is an intersection
  | otherwise =
      [(intersection, Just entry)]
        <> ([(Range (start range) (sourceStart entry - 1), Nothing) | start range < sourceStart entry])
        <> ([(Range (sourceEnd entry + 1) (end range), Nothing) | sourceEnd entry < end range])
  where
    maxStart = max (start range) (sourceStart entry)
    minEnd = min (end range) (sourceEnd entry)
    intersection = Range maxStart minEnd

findMinLocation :: [Mapping] -> [Range] -> Int
findMinLocation maps ranges = minimum $ start <$> foldl (\rs es -> concatMap (applyMappingToRange es) rs) ranges maps

main :: IO ()
main = do
  s : ms <- splitOn "\n\n" <$> readFile "05.txt"
  let go = findMinLocation (parseMappings ms)
      ints = parseSeedInts s
  print $ go $ intsToSeeds ints
  print $ go $ intsToRanges ints
