module Day10 where
    import Data.List (elemIndices, elemIndex, nub, delete)
    import Data.Tuple (swap)
    import Data.Maybe (catMaybes)
    import qualified Data.Map.Strict as Map

    type Vector = (Int, Int)

    toCoords :: String -> [Vector]
    toCoords str = fmap swap $ concat $ zipWith c [0..] $ fmap (fmap fromIntegral . (elemIndices '#')) $ lines str
        where c y = zip (repeat y)

    vAngle :: Vector -> Vector -> Vector
    vAngle (x1, y1) (x2, y2) = simplify (x1-x2, y1-y2)
    
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

    angleToDegree :: Vector -> Double
    angleToDegree v@(x, y) = extra + (180/pi) * (acos c)
        where
            vf = (fromIntegral x, fromIntegral y)
            up = (0, -1)
            dot (x1, y1) (x2, y2) = x1*x2 + y1*y2
            l (x, y) = sqrt $ x^2 + y^2
            c = dot up vf / ((l up) * (l vf))
            extra
                | x < 0 = 180
                | otherwise = 0
    
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
            m = Map.fromListWith (++) (coordToKV <$> coords)
            fireFrom map = (catMaybes $ Map.elems $ Map.map head' map) ++ fireFrom (Map.map tail' map)
    main = do
        map <- readFile "10.txt"
        let (origin, count) = part1 map
        print $ count
        print $ (fire origin map) !! 199

    t = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
    origin = (11, 13)
    order = (fire origin t)