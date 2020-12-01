module Day01 where
    p1 :: [Int] -> Int
    p1 xs = head $ [a*b | a <- xs, b <-xs, a+b==2020]

    p2 :: [Int] -> Int
    p2 xs = head $ [a*b*c | a <- xs, b <-xs, c <-xs, a+b+c==2020]

    main = do
        input <- (fmap read) <$> lines <$> readFile "01.txt"
        print (p1 input)
        print (p2 input)