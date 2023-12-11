module Day10 where

import Control.Monad (join)
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (groupBy, intersect, sort)
import Data.Matrix qualified as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

data Dir = N | E | S | W deriving (Show, Eq, Enum, Bounded)

invert :: Dir -> Dir
invert N = S
invert S = N
invert E = W
invert W = E

move :: Dir -> (Int, Int) -> (Int, Int)
move N (i, j) = (i - 1, j)
move S (i, j) = (i + 1, j)
move E (i, j) = (i, j + 1)
move W (i, j) = (i, j - 1)

getDirs :: Char -> [Dir]
getDirs '-' = [E, W]
getDirs '|' = [N, S]
getDirs 'L' = [N, E]
getDirs '7' = [S, W]
getDirs 'J' = [N, W]
getDirs 'F' = [E, S]
getDirs '.' = []

rotate :: Char -> Dir -> Dir
rotate 'L' S = E
rotate 'L' W = N
rotate '7' N = W
rotate '7' E = S
rotate 'J' S = W
rotate 'J' E = N
rotate 'F' N = E
rotate 'F' W = S
rotate '|' d = d
rotate '-' d = d

-- rotate _ d = d

at :: M.Matrix Char -> (Int, Int) -> Char
at m (i, j) = fromMaybe '.' $ M.safeGet i j m

findAndReplaceS :: M.Matrix Char -> ((Int, Int), M.Matrix Char)
findAndReplaceS m = (startPos, m')
  where
    startPos = fromJust $ asum $ M.toList $ M.mapPos (\i ch -> if ch == 'S' then Just i else Nothing) m
    isStartConnectedTo :: Dir -> Bool
    isStartConnectedTo d = invert d `elem` getDirs (m `at` move d startPos)
    connectedDirections = filter isStartConnectedTo [N, E, S, W]
    startPipe = head $ filter (\pipe -> connectedDirections == getDirs pipe) ['-', '|', 'L', '7', 'J', 'F']
    m' = M.setElem startPipe startPos m

step :: M.Matrix Char -> ((Int, Int), Dir) -> ((Int, Int), Dir)
step m (pos, dir) = (pos', dir')
  where
    currentPipe = m `at` pos
    dir' = rotate currentPipe dir
    pos' = move dir' pos

walk :: M.Matrix Char -> ((Int, Int), Dir) -> [((Int, Int), Dir)]
walk m = iterate (step m)

getLoop :: (Int, Int) -> M.Matrix Char -> [(Int, Int)]
getLoop startPos matrix = map fst $ takeWhile (\(p, _) -> p /= startPos) . tail $ walk matrix (startPos, startDir)
  where
    startDir = invert $ head $ getDirs $ matrix `at` startPos

part1 :: [(Int, Int)] -> Int
part1 loop = 1 + length loop `div` 2

part2 :: M.Matrix Char -> [(Int, Int)] -> Int
part2 matrix loop = length $ join innerTiles
  where
    rows = groupBy ((==) `on` fst) $ sort loop
    northPipes = map (filter (\p -> N `elem` getDirs (matrix `at` p))) rows
    innerTiles = zipWith getInnerTilesForRow northPipes rows

getInnerTilesForRow :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
getInnerTilesForRow northPipes loopRow = filter (`notElem` loopRow) $ join innerTiles
  where
    innerTiles = skipEven $ zipWith (\(i, j1) (_, j2) -> [(i, j) | j <- [j1 .. j2]]) northPipes (tail northPipes)
    skipEven (x : _ : xs) = x : skipEven xs
    skipEven xs = xs

main :: IO ()
main = do
  input <- lines <$> readFile "10.txt"
  let (startPos, matrix) = findAndReplaceS $ M.fromLists input
      loop = getLoop startPos matrix
  print $ part1 loop
  print $ part2 matrix $ startPos : loop
