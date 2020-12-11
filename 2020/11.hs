module Day11 where

import Data.Bifunctor (bimap)
import qualified Data.Matrix as M
import Data.Maybe (mapMaybe, isJust, fromJust)

step, step2 :: M.Matrix Char -> (Int, Int) -> Char -> Char

step _ _ '.' = '.'
step m (x,y) ch
    | ch == 'L' && countTaken == 0 = '#'
    | ch == '#' && countTaken >= 4 = 'L'
    | otherwise = ch
    where 
        pos = [(a,b) | a <- [x-1, x, x+1], b <- [y-1,y,y+1], a /= x || b /= y]
        countTaken = length $ filter (== '#') $ mapMaybe (\(a,b) -> M.safeGet a b m) pos

step2 _ _ '.' = '.'
step2 m (x,y) ch
    | ch == 'L' && countTaken == 0 = '#'
    | ch == '#' && countTaken >= 5 = 'L'
    | otherwise = ch
    where 
        dir x y = bimap (+x) (+y)
        offs = [-1, 0, 1]
        firstInDirection d = take 1 $ filter (/= '.') $ map fromJust $ takeWhile isJust $ tail $ (\(a,b) -> M.safeGet a b m) <$> iterate d (x,y)
        countTaken = length $ filter (== '#') $ [dir a b | a <- offs, b <- offs, a /= 0 || b /= 0] >>= firstInDirection

converge :: Eq a => (a -> a) -> a -> a -- https://stackoverflow.com/a/23924238
converge = until =<< ((==) =<<)

main = do
    inp <- lines <$> readFile "11.txt"
    let m = M.fromLists inp
        runStep s = length $ filter (== '#')$ M.toList $ converge (\m' -> M.mapPos (s m') $ m') $ m
    print $ runStep step
    print $ runStep step2