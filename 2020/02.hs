module Day02 where
    import Text.Parsec
    import Text.Parsec.Token
    import Control.Monad

    type Pass = (Int, Int, Char, String)

    pw = do -- Yep, parsec.
        a <- r <$> many1 digit
        char '-'
        b <- r <$> many1 digit
        space
        c <- anyChar
        char ':' >> space
        d <- many1 alphaNum
        return (a,b,c, d)
        where r = read :: String -> Int
    
    correct1 :: Pass -> Bool
    correct1 (mi, ma, ch, p) = b $ length $ filter (==ch) p
        where b x = x >= mi && x <= ma 
    
    correct2 :: Pass -> Bool
    correct2 (a, b, ch, p) = if pa then not pb else pb
        where 
            pa = p !! (a-1) == ch
            pb = p !! (b-1) == ch

    part :: (Pass -> Bool) -> [Pass] -> Int
    part c = (foldl (\i v -> if v then i + 1 else i) 0) . (fmap c)

    main = do
        input <- readFile "02.txt"
        print $ part correct1 <$> parse (sepBy pw (char '\n')) "" input
        print $ part correct2 <$> parse (sepBy pw (char '\n')) "" input