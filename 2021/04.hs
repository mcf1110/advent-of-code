{-# LANGUAGE TupleSections #-}

module Day04 where

import Data.Foldable (find)
import Data.List.Split
import qualified Data.Matrix as M
import qualified Data.Vector as V

type Board = M.Matrix (Int, Bool)

main :: IO ()
main = do
  (ds : _ : bs) <- lines <$> readFile "04.txt"
  let draws = parseDraws ds
      boards = parseBoards bs
  print $ getScore $ playBingoUntilFirstWins draws boards
  print $ getScore $ playBingoUntilLastWins draws boards

parseDraws :: String -> [Int]
parseDraws = map read . splitOn ","

parseBoards :: [String] -> [Board]
parseBoards = map (M.fromLists . map (map ((,False) . read) . words)) . splitOn [""]

mark :: Int -> Board -> Board
mark draw = fmap (\(val, checked) -> (val, checked || draw == val))

bingoStep :: Int -> [Board] -> [Board]
bingoStep d = fmap (mark d)

getRows :: M.Matrix a -> [V.Vector a]
getRows m = [M.getRow n m | n <- [1 .. M.nrows m]]

getCols :: M.Matrix a -> [V.Vector a]
getCols m = [M.getCol n m | n <- [1 .. M.ncols m]]

isWinner :: Board -> Bool
isWinner b = any (all snd) (getRows b) || any (all snd) (getCols b)

playBingoUntilFirstWins :: [Int] -> [Board] -> (Board, Int)
playBingoUntilFirstWins (d : ds) bs =
  let newBoards = bingoStep d bs
   in case find isWinner newBoards of
        Just b -> (b, d)
        Nothing -> playBingoUntilFirstWins ds newBoards
playBingoUntilFirstWins _ _ = undefined

playBingoUntilLastWins :: [Int] -> [Board] -> (Board, Int)
playBingoUntilLastWins (d : ds) bs =
  let newBoards = bingoStep d bs
   in case filter (not . isWinner) newBoards of
        [b] -> playBingoUntilFirstWins ds [b] -- play until it wins :D
        losers -> playBingoUntilLastWins ds losers
playBingoUntilLastWins _ _ = undefined

getScore :: (Board, Int) -> Int
getScore (b, i) = i * sum (map fst $ filter (not . snd) $ M.toList b)
