module Day23 where
import Data.List
import Data.Maybe (fromJust)

move :: Int -> [Int] -> [Int]
move l (cur:p1:p2:p3:cups) = prefix <> [next,p1,p2,p3] <> postfix <> [cur]
    where tgt = fromJust $ find (not . (`elem` [p1,p2,p3])) $ filter (>=1) $ ([cur-1, cur-2, cur-3] <> [l,l-1..])
          (prefix, xs) = span (/= tgt) cups
          (next:postfix) = xs

part1 :: [Int] -> String
part1 inp = concat $ map show $ tail $ take l $ dropWhile (/=1) $ cycle $ (iterate (move (length inp)) inp) !! 100
    where l = length inp

million = 1000 * 1000

mkPart2 :: [Int] -> [Int]
mkPart2 xs = let m = maximum xs + 1 in xs <> [m..million]

part2 :: [Int] -> [Int]
part2 inp = take 2 $ tail $ take l $ dropWhile (/=1) $ cycle $ (iterate (move million) inp) !! 100 -- (10*million)
    where l = length inp

main = do
    inp <- map (read . (:[])) <$> readFile "23.txt"
    putStrLn $ part1 inp
    print $ part2 $ mkPart2 inp