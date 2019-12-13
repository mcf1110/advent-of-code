module Day13 where

    import Data.List.Split (splitOn, chunksOf)
    import qualified Data.Map as M
    import qualified IntCode as IC
    import qualified Data.Matrix as Mx

    import Debug.Trace

    t = [1,2,3,6,5,4]

    parse :: [Int] -> M.Map (Int, Int) (Int)
    parse is = M.fromList $ map f $ chunksOf 3 $ is
        where f (a:b:c:[]) = ((a,b), c)
    
    part1 :: String -> Int
    part1 pg = length $ filter (==2) $ M.elems $ parse $ IC.run pg []

    tile :: Int -> Char
    tile 0 = ' '
    tile 1 = '#'
    tile 2 = 'X'
    tile 3 = '_'
    tile 4 = '0'
    tile x = 'p'

    draw :: M.Map (Int, Int) (Int) -> String
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

    toIO :: M.Map (Int, Int) (Int) -> IC.ProgramState -> IO ()
    toIO map pg = do
        let m = IC.runUntilAsksForInput pg
        case m of
            Nothing -> print "GAME END"
            Just (pg', o) -> do
                let map' = M.union (parse o) map
                putStr $ "SCORE: "
                print $ map' M.! (-1, 0)
                putStr $ draw map'
                inp <- getChar
                getChar
                putStr "\ESC[2J"
                toIO map' $ IC.addInputs [cmd inp] pg'

    main = do
        pg <- readFile "13.txt"
        -- print $ part1 pg
        putStrLn "Use ASD + Enter to play:"
        let pgFree = '2':(tail pg)
        toIO M.empty $ IC.strToProgram pgFree []