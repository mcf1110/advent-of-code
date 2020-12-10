module Day10 where
import Data.List (sort, nub)
import qualified Data.Map.Strict as M

data Zipper a = Zipper {previous:: [a], focus :: a, next :: [a]} deriving Show

zipperFromList :: [a] -> Zipper a
zipperFromList (x:xs) = Zipper [] x xs
zipperToList :: Zipper a -> [a]
zipperToList (Zipper p f n) = reverse p ++ f:n

jmp :: Int -> Zipper a -> Zipper a
jmp 0 z = z
jmp x (Zipper p f n)
    | x > 0     = jmp (x-1) $ Zipper (f:p) (head n) (tail n)
    | otherwise = jmp (x+1) $ Zipper (tail p) (head p) (f:n)

cnt list = go M.empty list
    where 
        go m [] = m
        go m (x:xs) = go (M.alter incrIfExists x m) xs
        incrIfExists = Just . (maybe 1 (+1))

arrangements :: [Int] -> [[Int]]
arrangements ls = go $ jmp 1 $ zipperFromList $ ls
    where
        go z@(Zipper _ _ []) = [zipperToList $ z]
        go z@(Zipper (p:ps) _ (n:ns))
            | n - p <= 3 = [(jmp 1 z), (Zipper (p:ps) n ns)]  >>= go
            | otherwise = [(jmp 1 z)] >>= go

isRemovable acc (p:c:n:xs)
    | (n - p <= 3) = isRemovable (n:c:p:acc) (c:n:xs)
    | otherwise = (reverse $ nub acc):(isRemovable [] (c:n:xs))
isRemovable acc _ = []

interestingBits ls = filter (not . null) $ isRemovable [] ls

main = do
    inp <- sort . (fmap read) . lines <$> readFile "10.txt"
    let outlet = 0
        device = 3 + maximum inp
        adapters = outlet:inp ++ [device]
        diffs = zipWith (-) (tail adapters) adapters 
        counter = cnt diffs
    print $ (counter M.! 1) * (counter M.! 3)
    -- would work too, but takes too long
    -- print $ length $ arrangements adapters
    -- so we keep only the bits where at least one adapter can be discarded
    print $ product $ (length . arrangements) <$> interestingBits adapters