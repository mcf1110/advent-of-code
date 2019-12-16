module Day14 where

    import Data.List.Split (splitOn)
    import Debug.Trace

    import qualified Data.Map.Strict as M

    reactions = "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

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
    getMinCost rs sp e@(amt, elName) = case sp M.!? (traceShowId elName) of
        Nothing -> cost $ head rs'
        Just n -> if n >= amt 
            then (0, M.adjust (\x -> x - amt) elName sp) 
            else cost $ head rs'
        where 
            rs' = rs ! e
            cost (ins, (produced, el)) = traceShowId $ (c, m')
                where 
                    f el (c1, sp') = (\(c2, sp'') -> (c1+c2, sp'')) (getMinCost rs sp' el)
                    (c, m) = foldr f (0, sp) ins -- getMinCost rs sp
                    m' = M.unionWith (+) m $ M.singleton el (produced-amt)
    
    parse :: String -> [Reaction]
    parse = (map toReaction) . lines

    main = do
        print $ getMinCost (parse reactions) M.empty (1, "FUEL")


    -- IDEIAS:
    -- Usar um Map (Elemento => Quantidade) para representar a quantidade de elementos reserva
    -- OBS: teríamos um Map para cada Combinação de reações possível