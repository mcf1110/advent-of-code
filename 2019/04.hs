module Day04 where
    isIncreasing :: String -> Bool
    isIncreasing (a:b:cs) = a <= b && isIncreasing (b:cs)
    isIncreasing _ = True

    hasDouble :: String -> Bool
    hasDouble (a:b:cs) = a == b || hasDouble (b:cs)
    hasDouble _ = False
    
    hasDoubleButNoMore :: String -> Bool
    hasDoubleButNoMore [] = False
    hasDoubleButNoMore (x:xs) = go 1 x xs
        where
            go c _ [] = c == 2
            go count last (x:xs)
                | last == x = go (count + 1) last xs
                | count == 2 = True
                | otherwise = go 1 x xs

    main = do
        let possibleInputs = [236491..713787]
        print $ length $ filter (\x -> isIncreasing x && hasDouble x ) $ show <$> possibleInputs
        print $ length $ filter (\x -> isIncreasing x && hasDoubleButNoMore x ) $ show <$> possibleInputs