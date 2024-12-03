
module Day01 where
import Data.List
import Data.Maybe ( catMaybes )

import qualified Data.Map.Strict as M
import qualified Data.Set as S

part1 :: [Int] -> [Int] -> Int
part1 left right = sum $ map abs $ zipWith (-) (sort right) (sort left)

-- part2 :: [Int] -> [Int] -> Int
part2 left right = similarity $ foldl alg M.empty right
  where
    keys = S.fromList left
    similarity :: M.Map Int Int -> Int
    similarity m = sum $ catMaybes $ map (\x -> (x *) <$> M.lookup x m) left

    alg :: M.Map Int Int -> Int -> M.Map Int Int
    alg m k | not $ k `elem` keys = m
    alg m k = M.alter f k m

    f Nothing = return 1
    f (Just n) = return (n + 1)

            


main :: IO ()
main = do
  [left, right] :: [[Int]] <- transpose <$> map (map read . words) <$> lines <$> readFile "01.txt"
  print $ part1 left right
  print $ part2 right left