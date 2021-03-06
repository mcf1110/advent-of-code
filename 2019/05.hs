module Day05 where
    import Data.List.Zipper
    import Data.List (findIndex, intersperse)
    import Control.Monad (sequence)

    -- import Debug.Trace
    
    -- HELPERS --
    at :: Int -> Zipper a -> Zipper a
    at pos z = (!! pos) $ iterate right $ start z

    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

    -- MEMORY --
    type Memory = (Int, Zipper Int)

    strToMemory :: String -> Memory
    strToMemory = (,) 0 . fromList . map read . wordsWhen (==',')

    memoryToList :: Memory -> [Int]
    memoryToList (_, z) = toList z

    goto :: Int -> Memory -> Memory
    goto i (pos, z) = (i, at i z)

    moveMemory :: Int -> Memory -> Memory
    moveMemory i m@(pos, _) = goto (pos + i) m

    replaceMemoryWith :: Int -> Int -> Memory -> Memory
    replaceMemoryWith src dest (p, z) = (p, at p $ replace src $ at dest z)

    data Par = Pos Int | Imm Int deriving Show

    toPos :: Par -> Par
    toPos (Imm i) = Pos i
    toPos x = x

    parToInt :: Memory -> Par -> Int
    parToInt _ (Imm i) = i
    parToInt (_, z) (Pos i) = cursor $ at i z

    -- PARSING INSTRUCTIONS --

    data Instruction = 
        Add Par Par Par | 
        Mult Par Par Par |
        Out Par |
        In Par |
        JumpIfTrue Par Par |
        JumpIfFalse Par Par |
        LessThan Par Par Par |
        Equals Par Par Par |
        Halt deriving Show

    parseList :: [Int] -> Instruction
    parseList [] = Halt
    parseList xs@(ins:_) = case opcode of
        1 -> Add p1 p2 ( toPos p3 )
        2 -> Mult p1 p2 ( toPos p3 )
        3 -> In ( toPos p1 )
        4 -> Out p1
        5 -> JumpIfTrue p1 p2
        6 -> JumpIfFalse p1 p2
        7 -> LessThan p1 p2 ( toPos p3 )
        8 -> Equals p1 p2 ( toPos p3 )
        99 -> Halt
        where 
            par pos = if (last . show $ ins `div` (10 ^ (pos+1))) == '0' then Pos (xs !! pos) else Imm (xs !! pos)
            opcode = ins `rem` 100
            [p1, p2, p3] = par <$> [1..3]

    parseMemory :: Memory -> Instruction
    parseMemory (_, Zip _ b) = parseList b

    type Inputs = [Int]
    type Outputs = [Int]
    type ProgramState = (Memory, Inputs, Outputs)

    -- EXECUTING INSTRUCTIONS --

    applyInstruction :: Instruction -> ProgramState -> Maybe ProgramState
    applyInstruction (Add p1 p2 p3) (m, i, o) = Just (applyAndReplace (+) m p1 p2 p3, i, o)
    applyInstruction (Mult p1 p2 p3) (m, i, o) = Just (applyAndReplace (*) m p1 p2 p3, i, o)
    applyInstruction Halt _ = Nothing
    applyInstruction (Out p1) (m, i, o) = Just (moveMemory 2 m, i, (parToInt m p1):o)
    applyInstruction (In (Pos p)) ((pos, z), (i:is), o) = Just (m', is, o)
            where 
                pos' = pos + 2
                z' = at pos' $ replace i $ at p z
                m' = (pos', z')
    applyInstruction (JumpIfTrue p1 p2) pgSt = jumpIf (/=0) p1 p2 pgSt
    applyInstruction (JumpIfFalse p1 p2) pgSt = jumpIf (==0) p1 p2 pgSt
    applyInstruction (LessThan p1 p2 (Pos p)) pgSt = writeIf (<) p1 p2 p pgSt
    applyInstruction (Equals p1 p2 (Pos p)) pgSt = writeIf (==) p1 p2 p pgSt
 
    applyAndReplace :: (Int -> Int -> Int) -> Memory -> Par -> Par -> Par -> Memory
    applyAndReplace f m@(pos, z) p1 p2 (Pos i3) = (pos', z')
        where 
            pos' = pos + 4
            [i1, i2] = parToInt m <$> [p1, p2]
            z' = at pos' $ replace (f i1 i2) $ at i3 z

    jumpIf :: (Int -> Bool) -> Par -> Par -> ProgramState -> Maybe ProgramState
    jumpIf pred par pos (m, i, o)
        | pred i1 = Just (goto p m, i, o)
        | otherwise = Just (moveMemory 3 m, i, o)
            where i1 = parToInt m par
                  p = parToInt m pos

    writeIf :: (Int -> Int -> Bool) -> Par -> Par -> Int -> ProgramState -> Maybe ProgramState
    writeIf pred p1 p2 pos (m, i, o) = Just (moveMemory 4 $ replaceMemoryWith val pos m, i, o)
            where [i1, i2] = parToInt m <$> [p1, p2]
                  val = if pred i1 i2 then 1 else 0

    -- RUNNING PROGRAM --

    runIteration :: ProgramState -> Maybe ProgramState
    runIteration pgSt@(m, i, o) = applyInstruction (parseMemory m) pgSt

    runProgram :: ProgramState -> (Memory, Outputs)
    runProgram pgSt@(m, i, o) = case runIteration pgSt of
        Just pgSt' -> runProgram pgSt'
        Nothing -> (m, o)
    
    run :: String -> Inputs -> IO ()
    run s i = print $ snd $ runProgram (strToMemory s, i, [])
    
    main :: IO()
    main = do 
        pgm <- readFile "./05.txt"
        run pgm [1]
        run pgm [5]

    -- DEBUG --

    -- debugProgram :: ProgramState -> (Memory, Outputs)
    -- debugProgram pgSt@(m, i, o) = case trace (debugState pgSt) $ runIteration pgSt of
    --     Just pgSt' -> debugProgram pgSt'
    --     Nothing -> (m, o)
    
    -- debugState :: ProgramState -> String
    -- debugState (m@(p, Zip l r), i, o) = 
    --     (concat $ intersperse "  " $ show <$> reverse l)
    --     ++
    --     (" [" ++ show (head r) ++ "] " )
    --     ++
    --     (concat $ intersperse "  " $ show <$> tail r)
    --     ++ "\n" ++ (show $ parseMemory m) ++ "\n"

    -- test :: String -> Inputs -> IO()
    -- test s i = do
    --     let n = (\(_, Zip _ l) -> length l) $ strToMemory s
    --     putStrLn ""
    --     putStrLn $ take (n*2) $ repeat '-'
    --     putStrLn $ take (n*2) $ repeat '-'
    --     putStrLn $ "Inputs: " ++ (show i)
    --     putStrLn ""
    --     putStrLn $ take (n*2) $ repeat '-'
    --     putStrLn $ concat $ intersperse "  " $ show <$>  [0..(n-1)]
    --     putStrLn $ take (n*2) $ repeat '-'
    --     print $ snd $ debugProgram (strToMemory s, i, [])
    --     putStrLn ""