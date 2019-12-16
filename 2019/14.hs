module Day14 where

    import Data.List.Split (splitOn)
    import qualified Data.Map.Strict as M

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
    
    adjust :: Int -> Spare -> Reaction -> (Spare, [Element])
    adjust toProduce sp r@(_, (amt, elName)) = (sp', ins)
        where 
            spare = M.findWithDefault 0 elName sp
            ceil = ceiling $ (fromIntegral (toProduce - spare)) / (fromIntegral amt)
            (ins, (produced, _)) = r .* ceil
            sp' = M.insert elName (spare - (toProduce - produced)) sp
    
    (!) :: [Reaction] -> Element -> Reaction
    (!) rs (amt, el) = head $ filter ((el ==) . snd . snd) rs

    getCost :: [Reaction] -> Spare -> Element -> (Int, Spare)
    getCost _ spare (x, "ORE") = (x, spare)
    getCost rs spare e@(amt, _) = (c, m)
        where 
            (spare2, els) = adjust amt spare $ rs ! e
            f el (c1, sp) = (\(c2, sp') -> (c1+c2, sp')) (getCost rs sp el)
            (c, m) = foldr f (0, spare2) els
    
    parse :: String -> [Reaction]
    parse = (map toReaction) . lines

    main = do
        reactions <- readFile "14.txt"
        print $ fst $ getCost (parse reactions) M.empty (1, "FUEL")


    -- IDEIAS:
    -- Usar um Map (Elemento => Quantidade) para representar a quantidade de elementos reserva
    -- OBS: teríamos um Map para cada Combinação de reações possível