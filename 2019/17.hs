module Day17 where
    import qualified Data.Matrix as Mx
    import qualified Data.Set as S
    import qualified IntCode as IC
    import Data.List (inits, tails)

    import Debug.Trace
    import Data.Maybe (fromMaybe)

    type Coord = (Int, Int)

    toMatrix :: String -> Mx.Matrix Char
    toMatrix = Mx.fromLists . (filter (not . null)) . lines

    findCrossroads :: Mx.Matrix Char -> [Coord]
    findCrossroads mx = filter isCrossroad [(x,y) | x<-w, y<-h]
        where
            w = [2..(Mx.nrows mx - 1)]
            h = [2..(Mx.ncols mx - 1)]
            getCross (x,y) = [(x,y), (x+1,y), (x-1,y), (x,y+1), (x,y-1)]
            isCrossroad c = all (=='#') $ map (mx Mx.!) $ getCross c
    
    adjustment :: [Coord] -> Int
    adjustment is = sum $ map (\(a,b) ->(a-1) * (b-1)) is

    part1 = adjustment . findCrossroads . toMatrix

    t = "#######...#####\n#.....#...#...#\n#.....#...#...#\n......#...#...#\n......#...###.#\n......#.....#.#\n^########...#.#\n......#.#...#.#\n......#########\n........#...#..\n....#########..\n....#...#......\n....#...#......\n....#...#......\n....#####......"

    findChars :: Char -> String -> [Coord]
    findChars ch  str = [(x,y) | (y, row) <- zip [0..] l, (x, char) <- zip [0..] row, char == ch]
        where
            l = lines str

    findScaffolds :: String -> S.Set Coord
    findScaffolds = S.fromList . (findChars '#')
    
    createRobot :: String -> Robot
    createRobot str = Robot (head $ findChars '^' str) U

    data Direction = U | D | L | R deriving Show
    data Robot = Robot {coord :: Coord, direction :: Direction} deriving Show

    turnRight :: Robot -> Robot
    turnRight r = Robot (coord r) d
            where d = case direction r of
                        U -> R
                        R -> D
                        D -> L
                        L -> U
    
    turnLeft = turnRight . turnRight . turnRight
    
    move :: Direction -> Coord -> Coord
    move U (x, y) = (x, y-1)
    move D (x, y) = (x, y+1)
    move L (x, y) = (x-1, y)
    move R (x, y) = (x+1, y)

    moveRobot :: Robot -> Coord
    moveRobot (Robot c d) = move d c

    forward :: S.Set Coord -> Robot -> Maybe Robot
    forward cs r@(Robot _ d)
        | S.member c' cs = Just (Robot c' d)
        | otherwise = Nothing
        where 
            c' = moveRobot r


    isDeadEnd :: S.Set Coord -> Robot -> Bool
    isDeadEnd cs r = not $ any isScaffold [front, left, right]
        where 
            front = moveRobot r
            left = moveRobot $ turnLeft r
            right = moveRobot $ turnRight r
            isScaffold x = S.member x cs
    
    autoTurn :: S.Set Coord -> Robot -> (Robot, Char)
    autoTurn cs r = case forward cs right of
        Just _ -> (right, 'R')
        Nothing -> (turnLeft r, 'L')
        where
            right = turnRight r
    
    movesRobot :: S.Set Coord -> Robot -> String
    movesRobot cs r
            | isDeadEnd cs r = []
            | otherwise = case maybeR of
                Just newR -> 'f':(movesRobot cs newR)
                Nothing -> cmd:(movesRobot cs turned)
                where 
                    maybeR = forward cs r
                    (turned, cmd) = autoTurn cs r
    
    movesToCmd :: String -> [String]
    movesToCmd str = go 0 str
        where 
            go 0 [] = []
            go c [] = [show c]
            go c ('f':xs) = go (c+1) xs
            go 0 (x:xs) = [x]:(go 0 xs)
            go c (x:xs) = show c:[x]:(go 0 xs)
    
    countSublist :: [String] -> [String] -> Int 
    countSublist haystack needle
                | n > h = 0
                | otherwise = c + (countSublist (tail haystack) needle)
                where 
                    h = length haystack
                    n = length needle
                    slice = take n haystack
                    c = if slice == needle then 1 else 0

    removeSublist :: [String] -> [String] -> [String] 
    removeSublist haystack needle
                | n > h = haystack
                | slice == needle = removeSublist (drop n haystack) needle
                | otherwise = (head haystack):(removeSublist (tail haystack) needle)
                where 
                    h = length haystack
                    n = length needle
                    slice = take n haystack

    -- findLongestMove :: [String] -> [String]
    findLongestMove moves = (move, removeSublist t move)
        where 
            options = take 10 $ tail $ zip (tails moves) (inits moves)
            repetitions = map (uncurry countSublist) options
            f a b = if (snd b) == 0 then a else b
            ((t, move), _) = foldl1 f (zip options repetitions)

    main = do
        let r = createRobot t
        let s = findScaffolds t
        let moves = movesToCmd $ movesRobot s r

        print $ findLongestMove moves
        -- print $ countSublist moves <$> needles 
        -- pg <- readFile "17.txt"
        -- let x = (toEnum <$> IC.run pg []) :: String
        -- print $ part1 x
        -- putStrLn $ x
