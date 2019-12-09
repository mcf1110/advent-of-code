module Day08 where
    import Data.List.Split (chunksOf)
    import Data.List (elemIndices)

    type Layer = [[Int]]

    toLayers :: Int -> Int -> String -> [Layer]
    toLayers w h str = fmap (chunksOf w) $ chunksOf size $ ints
        where
            ints = (read . pure) <$> str :: [Int]
            size = w*h
    
    showL :: [Layer] -> String
    showL l = unlines $ fmap unlines $ fmap (fmap (fmap toChar)) l

    countDigitsOnLayer :: Int -> Layer -> Int
    countDigitsOnLayer d = (sum . (fmap (length . (filter (==d)))))
    
    layerOfFewestDigits :: Int -> [Layer] -> Int
    layerOfFewestDigits d ls = head $ elemIndices (minimum dCount) dCount
        where dCount = fmap (countDigitsOnLayer d) ls
    
    combinePixels :: Int -> Int -> Int
    combinePixels 2 x = x
    combinePixels x _ = x

    combineLayers :: Layer -> Layer -> Layer
    combineLayers = zipWith (zipWith combinePixels)

    toChar :: Int -> Char
    toChar 0 = ' '
    toChar _ = '█'

    main = do
        str <- readFile "08.txt"
        let ls = (toLayers 25 6 str)
        let layer = ls !! (layerOfFewestDigits 0 ls)
        print $ (countDigitsOnLayer 1 layer) * (countDigitsOnLayer 2 layer)
        putStr $ showL $ [foldl1 combineLayers ls]