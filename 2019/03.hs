module Day03 where
    import Data.Maybe (catMaybes)
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

    type Coord = (Int, Int)
    type Stroke = (Coord, Coord, Int)

    isBetween :: Int -> Int -> Int -> Bool
    isBetween x y z
        | x > z = isBetween z y x
        | otherwise = (x<=y)&&(y<=z)

    intersectStrokes :: Stroke -> Stroke -> Maybe (Coord, Int)
    intersectStrokes 
        h@((x1, y1), (x2, y2), ih) 
        v@((x3, y3), (x4, y4), iv)
            | x1 == x2 && y3 == y4 = intersectStrokes v h
            | x1 == x2 && x3 == x4 = Nothing -- parallel
            | y1 == y2 && y3 == y4 = Nothing -- parallel
            | isBetween x1 x3 x2 && isBetween y3 y1 y4= Just ((x3, y1), ink)
            | otherwise = Nothing
        where ink = ih + iv -(abs (x2-x3)) - ( abs (y4-y1))

    moveFrom :: Stroke -> String -> Stroke
    moveFrom (_, from, ink) (dr:ns) = (from, fn from, ink+n')
        where 
            fn = case dr of 'R' -> \(x, y) -> (x+n', y)
                            'L' -> \(x, y) -> (x-n', y)
                            'U' -> \(x, y) -> (x, y+n')
                            'D' -> \(x, y) -> (x, y-n')
            n' = (read ns)  :: Int

    toStrokes :: [String] -> [Stroke]
    toStrokes = go ((0,0), (0,0), 0)
        where 
            go _ [] = []
            go strk (cmd:cmds) = strk':(go strk' cmds)
                where
                    strk' = moveFrom strk cmd

    manhattan :: Coord -> Int
    manhattan (x, y) = abs x + abs y

    main = do 
        l <- lines <$> readFile "./03.txt"
        let [w1, w2] = toStrokes <$> wordsWhen (==',') <$> l

        let inters = tail $ catMaybes [intersectStrokes a b | a <- w1, b <- w2]
        putStr "Parte 1: "
        print $ minimum $ manhattan <$> fst <$> inters

        putStr "Parte 2: "
        print $ minimum $ snd <$> inters