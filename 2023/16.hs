module Day16 where

import Control.Monad (forM_, guard)
import Data.Bifunctor (Bifunctor (second))
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set qualified as S
import Prelude hiding (Left, Right)

data Mirror = Horizontal | Vertical | ForwardDiagonal | BackwardDiagonal deriving (Show, Eq)

type Beam = ((Int, Int), Direction)

data Direction = Up | Down | Left | Right deriving (Show, Eq, Ord)

type Grid = M.Map (Int, Int) Mirror

parseGrid :: String -> Grid
parseGrid = M.fromList . mapMaybe sequence . concat . zipWith (\y -> zipWith (\x c -> ((x, y), parse c)) [0 ..]) [0 ..] . lines
  where
    parse :: Char -> Maybe Mirror
    parse '-' = Just Horizontal
    parse '|' = Just Vertical
    parse '/' = Just ForwardDiagonal
    parse '\\' = Just BackwardDiagonal
    parse _ = Nothing

dimensions :: Grid -> (Int, Int)
dimensions g = (maximum $ map fst $ M.keys g, maximum $ map snd $ M.keys g)

reflect :: Mirror -> Direction -> [Direction]
reflect Horizontal Up = [Left, Right]
reflect Horizontal Down = [Left, Right]
reflect Vertical Left = [Up, Down]
reflect Vertical Right = [Up, Down]
reflect ForwardDiagonal Up = [Right]
reflect ForwardDiagonal Down = [Left]
reflect ForwardDiagonal Left = [Down]
reflect ForwardDiagonal Right = [Up]
reflect BackwardDiagonal Up = [Left]
reflect BackwardDiagonal Down = [Right]
reflect BackwardDiagonal Left = [Up]
reflect BackwardDiagonal Right = [Down]
reflect _ x = [x]

move :: Beam -> Beam
move ((x, y), Up) = ((x, y - 1), Up)
move ((x, y), Down) = ((x, y + 1), Down)
move ((x, y), Left) = ((x - 1, y), Left)
move ((x, y), Right) = ((x + 1, y), Right)

tick :: (Int, Int) -> Grid -> [Beam] -> [Beam]
tick (w, h) g bs = do
  (pos, dir) <- bs
  newDirection <- case M.lookup pos g of
    Just m -> reflect m dir
    Nothing -> [dir]
  let newBeam = move (pos, newDirection)
  guard $ inBounds (w, h) newBeam
  return newBeam

inBounds :: (Int, Int) -> Beam -> Bool
inBounds (w, h) ((x, y), _) = x >= 0 && x <= w && y >= 0 && y <= h

countEnergizedFrom :: Grid -> Beam -> Int
countEnergizedFrom input start = S.size $ S.map fst $ go S.empty [start]
  where
    runTick = tick (dimensions input) input
    go :: S.Set Beam -> [Beam] -> S.Set Beam
    go s [] = s
    go s (x : xs)
      | S.member x s = go s xs -- already seen this beam, don't bother
      | otherwise = go (S.insert x s) (runTick [x] ++ xs)

main :: IO ()
main = do
  input <- parseGrid <$> readFile "./16.txt"
  print $ countEnergizedFrom input ((0, 0), Right)

  let (w, h) = dimensions input
      allPossibleBeams = do
        x <- [0 .. w]
        y <- [0 .. h]
        d <- ([Right | x == 0]) <> ([Left | x == w]) <> ([Down | y == 0]) <> ([Up | y == h])
        return ((x, y), d)
  -- takes a while to run, but < 1 minute
  print $ maximum $ map (countEnergizedFrom input) allPossibleBeams