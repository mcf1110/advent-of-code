module Main where
import Data.List.Split (splitOn)
import qualified Data.Set as S

part1 :: [Int] -> [Int] -> [Int]
part1 (x:xs) (y:ys)
    | x > y = part1 (xs <> [x,y]) ys
    | otherwise = part1 xs (ys <> [y, x])
part1 a [] = a
part1 [] a = a

part2 states (x:xs) (y:ys) = case winner of
    (False, _) -> part2 states' (xs <> [x,y]) ys
    (True, _)  -> part2 states' xs (ys <> [y,x])
    where winner
            | (x:xs, y:ys) `S.member` states = (False, [])
            | length xs >= x && length ys >= y = part2 S.empty (take x xs) (take y ys)
            | x > y = (False, [])
            | otherwise = (True, [])
          states' = S.insert (x:xs, y:ys) states
part2 _ a [] = (False, a)
part2 _ [] a = (True, a)

main = do
    inp <- splitOn "\n\n" <$> readFile "22.txt"
    let [p1, p2]  = ((map read).tail.lines) <$> inp
    let score = sum . zipWith (*) [1..] . reverse
    print $ score $ part1 p1 p2
    print $ score $ snd $ part2 (S.empty) p1 p2 --takes a couple of minutes