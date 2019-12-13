module Day13 where

    import Data.List.Split (splitOn, chunksOf)
    import qualified Data.Map as M
    import qualified IntCode as IC
    import qualified Data.Matrix as Mx

    type Screen = M.Map (Int, Int) (Int)

    parse :: [Int] -> Screen
    parse is = M.fromList $ map f $ chunksOf 3 $ is
        where f (a:b:c:[]) = ((a,b), c)
    
    part1 :: String -> Int
    part1 pg = length $ filter (==2) $ M.elems $ parse $ IC.run pg []

    tile :: Int -> Char
    tile 0 = ' '
    tile 1 = 'T'
    tile 2 = '#'
    tile 3 = '_'
    tile 4 = '0'
    tile x = 'p'

    draw :: Screen -> String
    draw ps = unlines $ Mx.toLists $ go (M.toList ps) initM
        where
            w = maximum $ map fst $ M.keys ps
            h = maximum $ map snd $ M.keys ps
            initM = Mx.fromList (h+2) (w+2) (repeat ' ')
            go pos m = case pos of
                ((-1, 0), _):rest -> go rest m
                ((x, y), t):rest -> go rest $ Mx.setElem (tile t) (y+1, x+1) m
                [] -> m
    
    cmd :: Char -> Int
    cmd 'a' = -1
    cmd 'd' = 1
    cmd _ = 0

    toIO :: Screen -> IC.ProgramState -> IO ()
    toIO map pg = do
        let (m, o) = IC.runUntilAsksForInput pg
        let map' = M.union (parse o) map
        case m of
            Nothing -> (print $ draw $ map') >> putStr "GAME END"
            Just pg' -> do
                putStr $ "SCORE: "
                print $ map' M.! (-1, 0)
                putStr $ draw map'
                inp <- getChar
                getChar
                putStr "\ESC[2J"
                toIO map' $ IC.addInputs [cmd inp] pg'
    
    playerPosition :: Screen -> (Int, Int)
    playerPosition m = head $ M.keys $ M.filter (==3) m

    ballPosition :: Screen -> (Int, Int)
    ballPosition m = head $ M.keys $ M.filter (==4) m

    ballVelocity :: Screen -> Screen -> (Int, Int)
    ballVelocity s1 s2
                | M.null s1 = (1,1)
                | otherwise = (\(x0, y0) (x1, y1) -> (x1-x0, y1-y0)) (ballPosition s1) (ballPosition s2)
    score :: Screen -> Int
    score m = m M.! (-1, 0)
    
    sign :: Int -> Int
    sign 0 = 0
    sign x = if x > 0 then 1 else -1
    
    think :: Screen -> Screen -> Int
    think m0 m1
            | bx /= px = sign (bx-px)
            | by == (py - 1) = 0
            | otherwise = sign (vx)
        where
            (bx, by) = ballPosition m1
            (px, py) = playerPosition m1
            (vx, vy) = ballVelocity m0 m1

    autoPlay :: Screen -> IC.ProgramState -> Int
    autoPlay map pg = 
        let 
            (m, o) = IC.runUntilAsksForInput pg
            map' = M.union (parse o) map
        in
        case m of
            Nothing -> score map'
            Just pg' -> (autoPlay map' $ IC.addInputs [input] pg')
                where
                    input = think map map'
                
    main = do
        pg <- readFile "13.txt"
        print $ part1 pg
        let pgFree = '2':(tail pg)
        print $ autoPlay M.empty $ IC.strToProgram pgFree []