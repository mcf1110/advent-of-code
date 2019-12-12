module Day12 where
    import Data.Char (isDigit)
    import Data.Bifunctor (bimap)
    import qualified Data.Set as S

    type Vector = [Int]
    type Position = Vector
    type Velocity = Vector
    type Acceleration = Vector

    numeric :: Char -> Bool
    numeric x = x == '-' || isDigit x

    parsePos :: String -> Vector
    parsePos [] = []
    parsePos ('=':rest) = [read tk] ++ parsePos dp
        where(tk, dp) = span numeric rest
    parsePos (_:rest) = parsePos rest

    initialVelocity = [0,0,0]

    (.+.) :: Vector -> Vector -> Vector
    (.+.) = zipWith (+)

    (..+..) :: [Vector] -> [Vector] -> [Vector]
    (..+..) = zipWith (.+.)

    type State = ([Position], [Velocity])

    parse :: String -> State
    parse str = (parsePos <$> l, replicate (length l) initialVelocity)
        where l = lines str

    -- PART 1 --

    acc :: Position -> Position -> Acceleration
    acc pa pb = (\x -> -(x-1)) <$> fromEnum <$> zipWith compare pa pb
    
    applySingleGravity :: [Position] -> Position -> Acceleration
    applySingleGravity ps p = foldl (.+.) (repeat 0) $ map (acc p) ps

    gravity :: State -> State
    gravity (pos, vel) = (pos, vel ..+.. (fmap (applySingleGravity pos) pos))

    velocity :: State -> State
    velocity (pos, vel) = (pos ..+.. vel, vel)

    step :: State -> State
    step = velocity . gravity

    totalEnergy :: State -> Int
    totalEnergy (pos, vel) = sum $ zipWith (*) pot kin
        where 
            sumAbs x = sum $ (map abs) x
            pot = map sumAbs pos
            kin = map sumAbs vel

    -- PART 2 --
    extractCol :: Int -> [Vector] -> Vector
    extractCol a = map (!!a)

    findRepeatingAxis :: State -> Int -> Int
    findRepeatingAxis st a = length $ takeUntilDuplicate $ map (bimap ec ec) $ iterate step st
            where ec = extractCol a

    takeUntilDuplicate :: [(Position, Velocity)] -> [(Position, Velocity)]
    takeUntilDuplicate xs = foldr go (const []) xs S.empty
        where 
            go x cont set
                | S.member x set = []
                | otherwise = x : cont (S.insert x set)

    main = do
        str <- readFile "12.txt"
        let pos = parse str
        print $ totalEnergy $ (iterate step pos) !! 1000
        print $ foldl1 lcm $ findRepeatingAxis pos <$> [0,1,2]