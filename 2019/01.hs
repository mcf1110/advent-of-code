module Day01 where

    calculateFuel :: Integer -> Integer
    calculateFuel x = max 0 $ (floor $ (fromInteger x) / 3) - 2

    calculateExtraFuel :: Integer -> Integer
    calculateExtraFuel x 
        | fuel > 0 = fuel + (calculateExtraFuel fuel)
        | otherwise = fuel
        where fuel = calculateFuel x

    doPart :: (Integer -> Integer) -> IO ()
    doPart f = print =<< sum <$> (map $ f . read) <$> lines <$> readFile "./01.txt"

    part1 :: IO ()
    part1 = doPart calculateFuel
    
    part2 :: IO ()
    part2 = doPart calculateExtraFuel