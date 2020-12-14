module Day13 where
import Data.List.Split
import Data.List (sortBy, find)

part1 :: Integer -> [Integer] -> Integer
part1 target buses = busId * wait
    where 
        (busId, wait) = head $ sortBy (\(_, tw1) (_, tw2) -> compare tw1 tw2)  $ zip buses timeWaiting
        timeWaiting = zipWith (-) buses $ (target `mod`) <$> buses

-- Bruteforce approach
-- part2 buses = find isValid starts
--     where
--         offIds = tail $ map (fmap read) $ filter ((/= "x") . snd) $ zip [0..] buses :: [(Int, Int)]
--         base = read $ head buses
--         starts = map (*base) $ [0..]
--         isValid st = all (\(off, bid) -> (off + st) `mod` bid == 0) offIds

part2 buses = fst $ foldl1 f offIds
    where offIds = map (fmap read) $ filter ((/= "x") . snd) $ zip [0..] buses :: [(Int, Int)]
          f (start, step) (off, bid) = (start', step*bid)
              where start' = head $ filter (\x -> (x+off) `mod` bid == 0) $ [start + i*step | i<-[1..]]

main = do
    inp <- lines <$> readFile "13.txt"
    let target = read $ head inp
        buses = splitOn "," $ inp !! 1
        ignoreX = map read $ filter (/= "x") $ buses
    print $ part1 target ignoreX
    print $ part2 $ buses