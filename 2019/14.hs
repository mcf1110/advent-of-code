module Day14 where

    import Data.List.Split (splitOn)
    import Debug.Trace

    import qualified Data.Map.Strict as M

    reactions = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"

    type Element = (Int, String)
    type Reaction = ([Element], Element)
    type Spare = M.Map String Int

    toElement :: String -> Element
    toElement s = (read a, b)
        where [a,b] = words s

    toReaction :: String -> Reaction
    toReaction s = (es, toElement b)
        where 
            [as, b] = splitOn " => " s
            es = map toElement $ splitOn ", " as
    
    (.*) :: Reaction -> Int -> Reaction
    (.*) (ins, out) i = (map m ins, m out)
        where
            m (amt, el) = (amt * i, el) 
    
    adjust :: Int -> Reaction -> Reaction
    adjust toProduce r@(_, (amt, _)) = r .* a
        where a = ceiling $ (fromIntegral toProduce) / (fromIntegral amt)
    
    (!) :: [Reaction] -> Element -> [Reaction]
    (!) rs (amt, el) = map (adjust amt) $ filter ((el ==) . snd . snd) rs

    solve :: [(Int, Spare)] -> (Int, Spare)
    solve a = (sum i, foldr1 (M.unionWith (+)) s)
        where (i, s) = unzip a 

    getMinCost :: [Reaction] -> Spare -> Element -> (Int, Spare)
    getMinCost _ sp (x, "ORE") = (x, sp)
    -- getMinCost rs sp e@(amt, _) = minimum $ map cost rs'
    getMinCost rs sp e@(amt, elName) = case sp M.!? elName of
        Nothing -> cost $ head rs'
        Just n -> if n >= amt 
            then (0, M.adjust (\x -> x - amt) elName sp) 
            else cost $ head rs'
        where 
            rs' = rs ! e
            cost (ins, (produced, el)) = traceShow m' $ (c, m')
                where 
                    (c, m) = solve $ getMinCost rs sp <$> ins
                    m' = M.unionWith (+) m $ M.singleton el (produced-amt)
    
    parse :: String -> [Reaction]
    parse = (map toReaction) . lines

    main = do
        print $ getMinCost (parse reactions) M.empty (1, "FUEL")


    -- IDEIAS:
    -- Usar um Map (Elemento => Quantidade) para representar a quantidade de elementos reserva
    -- OBS: teríamos um Map para cada Combinação de reações possível