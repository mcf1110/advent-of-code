module Day06 where

howManyWaysToBeat :: (Int, Int) -> Int
howManyWaysToBeat (time, distance) = ceiling x1 - ceiling x0
  where
    -- -h^2 +th - d = 0
    delta = time ^ 2 - 4 * distance
    x0 = (fromIntegral time - sqrt (fromIntegral delta)) / 2
    x1 = (fromIntegral time + sqrt (fromIntegral delta)) / 2

fixKerning :: [Int] -> [Int] -> (Int, Int)
fixKerning times distances = (fix times, fix distances)
  where
    fix = read . concatMap show

main :: IO ()
main = do
  [times, distances] :: [[Int]] <- map (map read . tail . words) . lines <$> readFile "./06.txt"
  let races = zip times distances
  print $ product $ map howManyWaysToBeat races

  let bigRace = fixKerning times distances
  print $ howManyWaysToBeat bigRace