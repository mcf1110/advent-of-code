module Day11 where
    import qualified IntCode as IC
    import qualified Data.Map as M
    import Data.List (nub)
    
    type Pos = (Int, Int)
    data Color = Black | White deriving (Show, Eq)

    colorToInt c = if c == White then 1 else 0

    data Direction = U | R | D | L deriving Show
    type Robot = (Pos, Direction)

    instance Enum Direction where
        fromEnum U = 0
        fromEnum R = 1
        fromEnum D = 2
        fromEnum L = 3

        toEnum 0 = U 
        toEnum 1 = R 
        toEnum 2 = D 
        toEnum 3 = L 
        toEnum n = toEnum $ (4 + n) `mod` 4

    initRobot :: Robot
    initRobot = ((0,0), U)

    moveRobot :: Robot -> Robot
    moveRobot ((x,y), d) = case d of
        U -> ((x, y+1), d)
        D -> ((x, y-1), d)
        L -> ((x-1, y), d)
        R -> ((x+1, y), d)

    turnRobot :: Direction -> Robot -> Robot
    turnRobot newD (pos, d) = (pos, newD)

    turnRobotCW :: Robot -> Robot
    turnRobotCW r = turnRobot (succ $ snd r) r

    turnRobotCCW :: Robot -> Robot
    turnRobotCCW r = turnRobot (pred $ snd r) r

    type RobotState = (Robot, M.Map Pos Color)

    initState :: RobotState
    initState = (initRobot, M.empty)

    instruction :: (Int, Int) -> RobotState -> RobotState
    instruction (color, cw) (r, cmds) = (newR, newCmds)
        where 
            turn = if cw == 1 then turnRobotCW else turnRobotCCW
            newColor = if color == 0 then Black else White
            newR = moveRobot $ turn r
            newCmds = M.insert (fst r) newColor cmds

    executeIntCode :: IC.ProgramState -> Int -> Maybe (Int, Int, IC.ProgramState)
    executeIntCode pg input = do
        (pg1, o1) <- IC.runUntilOutput $ IC.addInputs [input] pg
        (pg2, o2) <- IC.runUntilOutput $ pg1
        return (o1, o2, pg2)

    test :: [(Int, Int)] -> RobotState
    test ins = foldl (flip instruction) initState ins

    runRobot :: String -> RobotState
    runRobot str = go (IC.strToProgram str []) initState
        where
            go pg rs = case executeIntCode pg (getInput rs) of
                Nothing -> rs
                Just (o1, o2, pg') -> go pg' $ instruction (o1, o2) rs
            getInput (r, cmds) = maybe 0 colorToInt $ cmds M.!? (fst r)

    part1 :: RobotState -> Int
    part1  = length . snd

    main = do
        str <- readFile "11.txt"
        print $ part1 $ runRobot str