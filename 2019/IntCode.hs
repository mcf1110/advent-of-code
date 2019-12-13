module IntCode where
    import qualified Data.Sequence as S
    import Data.List (findIndex, intersperse)
    import Control.Monad (sequence)
    import Data.Foldable (toList)
    -- HELPERS --

    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

    -- MEMORY --
    type Memory = (Int, S.Seq Int)

    strToMemory :: String -> Memory
    strToMemory = (,) 0 . S.fromList . map read . wordsWhen (==',')

    memoryToList :: Memory -> [Int]
    memoryToList (_, s) = toList s

    goto :: Int -> Memory -> Memory
    goto i (pos, s) = (i, s)

    moveMemory :: Int -> Memory -> Memory
    moveMemory i m@(pos, _) = goto (pos + i) m

    replaceMemoryWith :: Int -> Int -> Memory -> Memory
    replaceMemoryWith src dest (p, z) = (p, S.update dest src z)

    data Par = Pos Int | Imm Int | Rel Int deriving Show

    toPos :: Par -> Par
    toPos (Imm i) = Pos i
    toPos x = x

    getPosition :: Base -> Par -> Int
    getPosition b (Rel i) = (i + b)
    getPosition _ (Pos i) = i
    getPosition _ (Imm i) = error "GETTING POSITION FOR IMM"

    parToInt :: Base -> Memory -> Par -> Int
    parToInt _ _ (Imm i) = i
    parToInt _ (_, z) (Pos i) = S.index z i
    parToInt base m (Rel i) = parToInt base m (Pos (i+base))

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
        AdjustBase Par |
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
        9 -> AdjustBase p1
        99 -> Halt
        where 
            mode parPos = last . show $ ins `div` (10 ^ (parPos+1))
            par parPos = case mode parPos of
                '0' -> Pos (xs !! parPos) 
                '1' -> Imm (xs !! parPos)
                '2' -> Rel (xs !! parPos)
            opcode = ins `rem` 100
            [p1, p2, p3] = par <$> [1..3]

    parseMemory :: Memory -> Instruction
    parseMemory (i, s) = parseList $ drop i $ toList s

    type Inputs = [Int]
    type Outputs = [Int]
    type Base = Int
    type ProgramState = (Memory, Base, Inputs, Outputs)

    -- EXECUTING INSTRUCTIONS --

    applyInstruction :: Instruction -> ProgramState -> Maybe ProgramState
    applyInstruction (Add p1 p2 p3) (m, b, i, o) = Just (applyAndReplace (+) b m p1 p2 p3, b, i, o)
    applyInstruction (Mult p1 p2 p3) (m, b, i, o) = Just (applyAndReplace (*) b m p1 p2 p3, b, i, o)
    applyInstruction Halt _ = Nothing
    applyInstruction (Out p1) (m, b, i, o) = Just (moveMemory 2 m, b, i, (parToInt b m p1):o)
    applyInstruction (In p) ((pos, z), b, (i:is), o) = Just (m', b, is, o)
            where 
                pIndex = getPosition b p
                pos' = pos + 2
                z' = S.update pIndex i z
                m' = (pos', z')
    applyInstruction (JumpIfTrue p1 p2) pgSt = jumpIf (/=0) p1 p2 pgSt
    applyInstruction (JumpIfFalse p1 p2) pgSt = jumpIf (==0) p1 p2 pgSt
    applyInstruction (LessThan p1 p2 p) pgSt = writeIf (<) p1 p2 p pgSt
    applyInstruction (Equals p1 p2 p) pgSt = writeIf (==) p1 p2 p pgSt
    applyInstruction (AdjustBase p1) (m, b, i, o) = Just (moveMemory 2 m, b + (parToInt b m p1), i, o)
    applyInstruction a b = error $  "INSTRUCTION\n" ++ (show a) ++ "\n" ++ (show b)
 
    applyAndReplace :: (Int -> Int -> Int) -> Base -> Memory -> Par -> Par -> Par -> Memory
    applyAndReplace f b m@(pos, z) p1 p2 p3 = (pos', z')
        where 
            i3 = getPosition b p3
            pos' = pos + 4
            [i1, i2] = parToInt b m <$> [p1, p2]
            z' = S.update i3 (f i1 i2) z

    jumpIf :: (Int -> Bool) -> Par -> Par -> ProgramState -> Maybe ProgramState
    jumpIf pred par pos (m, b, i, o)
        | pred i1 = Just (goto p m, b, i, o)
        | otherwise = Just (moveMemory 3 m, b, i, o)
            where i1 = parToInt b m par
                  p = parToInt b m pos

    writeIf :: (Int -> Int -> Bool) -> Par -> Par -> Par -> ProgramState -> Maybe ProgramState
    writeIf pred p1 p2 pos (m, b, i, o) = Just (moveMemory 4 $ replaceMemoryWith val posIndex m, b, i, o)
            where 
                posIndex = getPosition b pos
                [i1, i2] = parToInt b m <$> [p1, p2]
                val = if pred i1 i2 then 1 else 0

    -- RUNNING PROGRAM --

    runIteration :: ProgramState -> Maybe ProgramState
    runIteration pgSt@(m, _, _, _) = applyInstruction (parseMemory m) pgSt

    runProgram :: ProgramState -> (Memory, Outputs)
    runProgram pgSt@(m, _, _, o) = case runIteration pgSt of
        Just pgSt' -> runProgram pgSt'
        Nothing -> (m, o)
    
    runUntilOutput :: ProgramState -> Maybe (ProgramState, Int)
    runUntilOutput pgSt@(_, _, _, []) = case runIteration pgSt of
        Just pgSt' -> runUntilOutput pgSt'
        Nothing -> Nothing
    runUntilOutput pgSt@(_, _, _, o:os) = Just (removeOutput pgSt, o)

    runUntilAsksForInput :: ProgramState -> Maybe (ProgramState, Outputs)
    runUntilAsksForInput pgSt = case runIteration pgSt of
        Nothing -> Nothing
        Just pgSt'@(m, _, [], o) -> case parseMemory m of
            In _ -> Just (removeOutput pgSt', reverse o)
            _ -> runUntilAsksForInput pgSt'
        Just pgSt' -> runUntilAsksForInput pgSt'

    removeOutput :: ProgramState -> ProgramState
    removeOutput (m, b, i, _) = (m, b, i, [])

    addInputs :: Inputs -> ProgramState -> ProgramState
    addInputs is (m, b, i, o) = (m, b, i ++ is, o)
    
    extendMem :: Memory -> Memory
    extendMem (p, seq) = (p, S.fromList $ (toList seq) ++ replicate 500 0)

    strToProgram :: String -> Inputs -> ProgramState
    strToProgram s i = (extendMem (strToMemory s), 0, i, [])

    run :: String -> Inputs -> Outputs
    run s = reverse . snd . runProgram . (strToProgram s)
