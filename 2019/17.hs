module Day17 where
    import qualified Data.Matrix as Mx
    import qualified Data.Set as S
    import qualified IntCode as IC
    import Data.List (inits, tails, intersperse)

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
    
    parseMoves :: String -> Moves
    parseMoves str = go 0 str
        where 
            go 0 [] = []
            go c [] = [show c]
            go c ('f':xs) = go (c+1) xs
            go 0 (x:xs) = [x]:(go 0 xs)
            go c (x:xs) = show c:[x]:(go 0 xs)

    data Routine = A | B | C deriving Show
    type Moves = [String]
    data Answer = Answer { rs :: [Routine], a :: Moves, b :: Moves, c :: Moves } deriving Show

    correct = Answer
                [A,B,A,C,A,B,C,A,B,C]
                ["R","8","R","10","R","10"]
                ["R","4","R","8","R","10","R","12"]
                ["R","12","R","4","L","12","L","12"]

    get :: Routine -> Answer -> Moves
    get A = a
    get B = b
    get C = c

    expand :: Answer -> Moves
    expand (Answer [] _ _ _) = []
    expand an@(Answer (x:xs) a b c) = get x an ++ (expand $ Answer (xs) a b c)

    commaSeparate :: [String] -> String
    commaSeparate = concat . (intersperse ",")

    toInput :: Answer -> String
    toInput (Answer rs a b c) = 
        (commaSeparate $ map show rs) ++ "\n" ++
        (commaSeparate $ a) ++ "\n" ++
        (commaSeparate $ b) ++ "\n" ++
        (commaSeparate $ c) ++ "\nn\n"

    main = do
        pg <- readFile "17.txt"
        let t = (toEnum <$> IC.run pg []) :: String
        
        putStr t
        putStrLn "Part 1:"
        print $ part1 t
        putStrLn "Part 2:"
        let r = createRobot t
            s = findScaffolds t
            moves = parseMoves $ movesRobot s r
            inputs = map fromEnum $ toInput $ correct
            pg' = '2':(tail pg)
            x = (IC.run pg' inputs)
        print $ last x
