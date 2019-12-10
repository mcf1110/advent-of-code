module Day10 where
    import Data.List (elemIndices, nub, delete, sortBy)
    import Data.Tuple (swap)
    import Data.Maybe (catMaybes)
    import Data.Ord (comparing)
    import Data.Bifunctor (bimap)
    import qualified Data.Map.Strict as Map

    type Vector = (Int, Int)

    toCoords :: String -> [Vector]
    toCoords str = fmap swap $ concat $ zipWith c [0..] $ fmap (fmap fromIntegral . (elemIndices '#')) $ lines str
        where c y = zip (repeat y)

    difference :: Vector -> Vector -> Vector
    difference (x1, y1) (x2, y2) = (x1-x2, y1-y2)
    
    vAngle :: Vector -> Vector -> Vector
    vAngle a b = simplify $ difference a b
    
    simplify :: Vector -> Vector
    simplify (x, y) = (x `div` l, y `div` l)
        where l = gcd x y

    visibleAsteroids :: [Vector] -> Vector -> Int
    visibleAsteroids vs v = length $ nub $ [vAngle v v' | v' <- vs, v' /= v]

    part1 :: String -> (Vector, Int)
    part1 str = (c !! i, m) 
        where 
            i = head $ elemIndices m va
            m = maximum va
            va = ((visibleAsteroids c) <$> c)
            c = toCoords str
    
    l (x, y) = sqrt $ x^2 + y^2

    angleToDegree :: Vector -> Double
    angleToDegree v = extra $ (180/pi) * (acos c)
        where
            vf = bimap fromIntegral fromIntegral v
            up = (0, -1)
            dot (x1, y1) (x2, y2) = x1*x2 + y1*y2
            c = dot up vf / ((l up) * (l vf))
            extra val
                | (fst v) < 0 = 360-val
                | otherwise = val
    
    head' :: [a] -> Maybe a
    head' [] = Nothing
    head' x = Just (head x)

    tail' :: [a] -> [a]
    tail' [] = []
    tail' x = tail x

    fire :: Vector -> String -> [Vector]
    fire origin str = fireFrom m
        where 
            coords = delete origin $ toCoords str
            coordToKV v = (angleToDegree (vAngle v origin), [v])
            distance v = l $ bimap fromIntegral fromIntegral $ difference v origin
            order = sortBy (comparing distance)
            m = Map.map order $ Map.fromListWith (++) (coordToKV <$> coords)
            fireFrom map = (catMaybes $ Map.elems $ Map.map head' map) ++ fireFrom (Map.map tail' map)
    main = do
        map <- readFile "10.txt"
        let (origin, count) = part1 map
        print $ count
        print $ (fire origin map) !! 199
