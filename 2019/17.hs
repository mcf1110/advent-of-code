module Day17 where
    import qualified Data.Matrix as Mx
    import qualified IntCode as IC

    toMatrix :: String -> Mx.Matrix Char
    toMatrix = Mx.fromLists . (filter (not . null)) . lines

    findCrossroads :: Mx.Matrix Char -> [(Int, Int)]
    findCrossroads mx = filter isCrossroad [(x,y) | x<-w, y<-h]
        where
            w = [2..(Mx.nrows mx - 1)]
            h = [2..(Mx.ncols mx - 1)]
            getCross (x,y) = [(x,y), (x+1,y), (x-1,y), (x,y+1), (x,y-1)]
            isCrossroad c = all (=='#') $ map (mx Mx.!) $ getCross c
    
    adjustment :: [(Int, Int)] -> Int
    adjustment is = sum $ map (\(a,b) ->(a-1) * (b-1)) is

    part1 = adjustment . findCrossroads . toMatrix

    main = do
        pg <- readFile "17.txt"
        let x = (toEnum <$> IC.run pg []) :: String
        print $ part1 x