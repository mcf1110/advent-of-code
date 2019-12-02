module Day02 where

    import Data.List.Zipper
    import Data.List (findIndex)

    type PosZip = (Int, Zipper Int)
    wordsWhen     :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

    parse :: String -> PosZip
    parse = (,) 0 . fromList . map read . wordsWhen (==',')
    
    at :: Int -> Zipper a -> Zipper a
    at pos z = (!! pos) $ iterate right $ start z

    runF :: (Int -> Int -> Int) -> PosZip -> PosZip
    runF f (p, z@(Zip _ (_:s1:s2:d:_))) = (p+4, z2)
        where 
            i = cursor $ at s1 z
            j = cursor $ at s2 z
            z1 = replace (f i j) $ at d z
            z2 = at (p+4) z1

    run :: PosZip -> PosZip
    run (p, z@(Zip _ (99:_))) = (p, z)
    run z@(p, (Zip _ (1:s1:s2:d:_))) = run $ runF (+) z
    run z@(p, (Zip _ (2:s1:s2:d:_))) = run $ runF (*) z
    run (p, z) = run (1+p, right z)

    go :: PosZip -> Int
    go = head . toList . snd . run


    prePart1 :: String -> PosZip
    prePart1 x = (0, start final)
        where 
            parsed = snd $ parse x
            inter = replace 12 $ at 1 parsed
            final = replace 2 $ at 2 inter
    
    runProgramWithInputs :: Int -> Int -> String -> Int
    runProgramWithInputs i1 i2 program = go (0, start final)
        where 
            parsed = snd $ parse program
            inter = replace i1 $ at 1 parsed
            final = replace i2 $ at 2 inter

    part1 :: IO()
    part1 = print =<< runProgramWithInputs 12 2 <$> readFile "./02.txt"

    part2 :: IO()
    part2 = do
        pg <- readFile "./02.txt"
        let outs = [runProgramWithInputs n v pg | n <- [0..99], v <- [0..99]]
        print $ findIndex (== 19690720) outs