module Day16 where
    import Data.Char (digitToInt)

    -- t = "03036732577212944063491565474664"
    
    pattern i = tail $ cycle $ concat $ map ((take (i+1)) . repeat) p
        where p = [0, 1, 0, -1]

    parse :: String -> [Int]
    parse = map digitToInt

    go :: [Int] -> [Int]
    go is = [(`mod` 10) $ abs $ sum $ zipWith (*) is (pattern i) | i<-[0..l] ]
        where l = length is - 1

    afterPhases :: [Int] -> Int -> [Int]
    afterPhases d i = (iterate go d) !! i

    main = do
        t <- readFile "16.txt"
        let d = parse t
        putStrLn $ concat $ map show $ take 8 $ afterPhases d 100

        let offset = read $ take 7 $ t :: Int
        let t2 = concat $ replicate 10000 t
        let final = afterPhases (parse t2)100
        putStrLn $ concat $ map show $ take 8 $ drop offset $ final