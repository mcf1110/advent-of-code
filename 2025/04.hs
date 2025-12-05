
module Day04 where
import qualified Data.Set as S

type Pos = (Int, Int)
type Grid = S.Set Pos

parse :: String -> Grid
parse = S.fromList . concatMap (\(y, vs) -> [(x,y) | (x,v) <- zip [0..] vs, v == '@']) . zip [0..] . lines

p8 :: Pos -> [Pos]
p8 (x,y) = [(a,b) | a <- [x-1, x, x+1], b<- [y-1, y, y+1], (a,b) /= (x,y)]



fewerThan :: Int -> Grid -> Pos -> Bool
fewerThan n g p = (<n) $ length $ filter (`S.member` g) $ p8 p

part1, part2 :: Grid -> Int
part1 grid = length $ S.filter (fewerThan 4 grid) grid
part2 grid
    | removedCount == 0 = 0
    | otherwise = removedCount + part2 removed
    where
        removedCount = S.size grid - S.size removed
        removed = S.filter (not . fewerThan 4 grid) grid

main :: IO ()
main = do
  x <- parse <$> readFile "04.txt"
  print $ part1 x
  print $ part2 x