module Day16 where
    import Data.Char (digitToInt)
    import Data.List (tails)
    
    pattern :: Int -> [Int]
    pattern i = tail $ cycle $ concat $ map ((take (i+1)) . repeat) p
        where p = [0, 1, 0, -1]

    patternNoLeading :: Int -> [Int]
    patternNoLeading i = cycle $ concat $ map ((take (i+1)) . repeat) p
        where p = [1, 0, -1, 0]

    parse :: String -> [Int]
    parse = map digitToInt

    calc :: [Int] -> [Int] -> Int
    calc a b = (`mod` 10) $ abs $ sum $ zipWith (*) a b

    go :: [Int] -> [Int]
    go is = [calc is (pattern i) | i<-[0..l] ]
        where l = length is - 1

    afterPhases :: [Int] -> Int -> [Int]
    afterPhases d i = (iterate go d) !! i

    goOffset :: Int -> [Int] -> [Int]
    goOffset i is = [calc x $ patternNoLeading p | (p, x) <- zip ps (tails is)] -- 
        where 
            l = length is
            ps = [i..(l+i-1)] :: [Int]

    main = do
        t <- readFile "16.txt"
        let d = parse t
        -- -- putStrLn $ concat $ map show $ take 8 $ afterPhases d 100

        let offset = read $ take 7 $ t :: Int
        print offset
        let t2 = drop offset $ concat $ replicate 10000 t
        print $ length t2
        putStrLn $ concat $ map show $ take 8 $ (iterate (goOffset offset) (parse t2)) !! 100