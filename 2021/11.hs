{-#LANGUAGE TupleSections#-}
module Day11 where

import qualified Data.Matrix as M
import Control.Monad (liftM2)
import Data.Monoid

type Level = Int
type Map = M.Matrix Level

main :: IO ()
main = do
  input <-((fmap (read . (:[]))) . M.fromLists . lines <$> readFile "11.txt") :: IO Map
  print $ fst $ runSteps 100 input
  print $ findSync input

findSync :: Map -> Int
findSync m = if i == 100 then 1 else (1+(findSync m'))
    where
        (i,m') = step m

runSteps :: Int -> Map -> (Int,Map)
runSteps v map = (iterate (\(i1,m1) -> let (i2, m2) = step m1 in (i1+i2, m2)) (0, map)) !! v

step :: Map -> (Int, Map)
step map = countShines $ go (liftM2 (,) [1..(M.nrows map)] [1..(M.ncols map)]) $ ((,False).succ) <$> map
    where
        go [] m = m
        go ((i,j):ps) m = case M.safeGet i j m of
            Nothing -> go ps m -- does not exist
            Just (_, True) -> go ps m -- has already shone
            Just (v, False) -> 
                if v <= 9 
                then go ps m -- not enough energy
                else go (ps<>(getNeighbors (i, j))) (shine (i,j) m) -- Shine on, you crazy diamond
        shine (i,j) m = increase (getNeighbors (i,j)) reset
            where
                reset = M.unsafeSet (0,True) (i,j) m
                increase [] state = state
                increase ((i,j):ns) state = case M.safeGet i j state of
                    Nothing -> increase ns state
                    Just (_, True) -> increase ns state
                    Just (v, b) -> increase ns (M.unsafeSet (v+1, b) (i,j) state)
        countShines m = (getSum $ foldMap (\(_, b) -> if b then Sum 1 else Sum 0) m, fst <$> m)

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (i,j) = [(r,c) | r <- [i-1,i,i+1], c <- [j-1, j, j+1], (r,c) /= (i,j)]