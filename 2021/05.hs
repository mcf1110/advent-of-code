{-# LANGUAGE TupleSections #-}

module Day05 where
import qualified Data.Map as M

type Point = (Int, Int)
type Segment = (Point, Point)
type Board = M.Map Point Int

main :: IO ()
main = do
  input <- lines <$> readFile "05.txt"
  let allSegments = parseSegment <$> input
      straights =  filter isStraight $ allSegments
  print $ countOverlaps straights
  print $ countOverlaps segments

countOverlaps :: [Segment] -> Int
countOverlaps = M.size . M.filter (>=2) . makeBoard . (stroke =<<) 

parseSegment :: String -> Segment
parseSegment str = (read ("("<>st<>")"), read ("("<>end<>")"))
    where
        [st, arrow, end] = words str

isStraight :: Segment -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

stroke :: Segment -> [Point]
stroke ((x1, y1), (x2, y2))
    | x1 == x2 = (x1,) <$> range y1 y2
    | y1 == y2 = (,y1) <$> range x1 x2
    | otherwise = zip (range x1 x2) (range y1 y2)

range :: Int -> Int -> [Int]
range x y
    | x > y = [x,(x-1)..y]
    | otherwise = [x..y]

makeBoard :: [Point] -> Board
makeBoard = foldr update M.empty
    where update v counts
            | M.member v counts = M.adjust succ v counts
            | otherwise         = M.insert v 1 counts