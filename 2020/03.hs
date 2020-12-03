module Day03 where
    import Data.Bifunctor
    get :: [String] -> (Int, Int) -> Char
    get hill (x, y) = hill !! y !! (x `mod` (length . head $ hill))

    testSlope :: [String] -> (Int, Int) -> Int
    testSlope hill (x, y) = length $ filter (=='#') $ map (get hill) $ takeWhile ((<(length hill)) . snd) $ iterate (bimap (+x) (+y)) $ (0,0)

    main = do
        i <- lines <$> readFile "03.txt"
        print $ testSlope i (3,1)
        print $ product $ map (testSlope i) [(1,1), (3,1), (5,1), (7,1), (1,2)]