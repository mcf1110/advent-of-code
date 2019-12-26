module Day18 where
    import qualified Data.Map.Strict as M
    import qualified Data.Set as Set
    import Data.Tree as T
    import Data.Char (isUpper, toUpper)
    import Debug.Trace

    type Coord = (Int, Int)
    data Block = Wall | Player | Key Char | Door Char | Empty deriving (Show, Eq)
    type Maze = M.Map Coord Block

    data Direction = U | R | D | L

    move :: Direction -> Coord -> Coord
    move U (x,y) = (x, y-1)
    move D (x,y) = (x, y+1)
    move R (x,y) = (x+1, y)
    move L (x,y) = (x-1, y)

    parseChar :: Char -> Block
    parseChar '#' = Wall
    parseChar '@' = Player
    parseChar '.' = Empty
    parseChar a
        | isUpper a = Door a
        | otherwise = Key (toUpper a)

    parse :: String -> Maze
    parse str = M.fromList [((x,y), parseChar ch) | (y, row) <- l, (x, ch) <- row]
        where 
            l = zip [0..] $ map (zip [0..]) $ lines str

    playerPosition :: Maze -> Coord
    playerPosition m = head $ M.keys $ M.filter (==Player) m
    
    possibleCoords :: Int -> Maze -> [(Coord, Int)]
    possibleCoords s m = snd $ go Set.empty s (playerPosition m)
            where go checked steps position 
                    | Set.member position checked = (checked, [])
                    | otherwise = case m M.!? position of
                        Nothing -> (newChecked, [])
                        Just Wall -> (newChecked, [])
                        Just (Door _) -> (newChecked, [])
                        Just (Key _) -> (newChecked, [(position, steps)])
                        Just _ -> (ch4, concat [r1,r2,r3,r4])
                        where
                            s = steps + 1
                            newChecked = Set.insert position checked
                            (ch1, r1) = go newChecked s (move L position)
                            (ch2, r2) = go ch1 s (move R position)
                            (ch3, r3) = go ch2 s (move U position)
                            (ch4, r4) = go ch3 s (move D position)
    
    movePlayerTo :: Coord -> Maze -> Maze
    movePlayerTo c m = case m M.! c of
        Key ch -> removePlayer $ updateCoord $ openDoor ch m
        _ -> removePlayer $ updateCoord m
        where
            removePlayer = M.insert (playerPosition m) Empty
            updateCoord = M.insert c Player
            openDoor ch mz = case coords of 
                    [] -> mz
                    _ -> M.insert (head coords) Empty mz
                where coords = M.keys $ M.filter (==(Door ch)) mz
        
    

    uf :: (Int, Maze) -> (Int, [(Int, Maze)])
    uf (i, m) = (i, map (\(c,s) -> (s, movePlayerTo c m)) $ possibleCoords i m)

    toTree :: String -> T.Tree Int
    toTree str = T.unfoldTree uf (0, parse str)

    minimumLeaf :: T.Tree Int -> Int
    minimumLeaf t = go maxBound t
        where
            go m (T.Node v []) = min m v
            go m (T.Node v f)
                | v >= m = m
                | otherwise = minimum $ map (go m) f

    main = do 
        str <- readFile "18.txt"
        let tree = toTree str
        -- putStr $ drawTree $ fmap show $ tree
        print $ minimumLeaf tree