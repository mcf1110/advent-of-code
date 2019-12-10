module Day10 where
    import Data.List (elemIndices, nub)
    import Data.Tuple (swap)

    type Vector = (Int, Int)

    toCoords :: String -> [Vector]
    toCoords str = fmap swap $ concat $ zipWith c [0..] $ fmap (fmap fromIntegral . (elemIndices '#')) $ lines str
        where c y = zip (repeat y)

    vAngle :: Vector -> Vector -> Vector
    vAngle (x1, y1) (x2, y2) = (x `div` l, y `div` l)
        where 
            (x, y) = (x1-x2, y1-y2)
            l = gcd x y

    visibleAsteroids :: [Vector] -> Vector -> Int
    visibleAsteroids vs v = length $ nub $ [vAngle v' v | v' <- vs, v' /= v]

    part1 :: String -> (Vector, Int)
    part1 str = (c !! i, m) 
            where 
                i = head $ elemIndices m va
                m = maximum va
                va = ((visibleAsteroids c) <$> c)
                c = toCoords str
    
    main = do
        map <- readFile "10.txt"
        print $ part1 map