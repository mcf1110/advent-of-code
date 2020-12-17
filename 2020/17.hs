module Day17 where
import qualified Data.Set as S

type Pos = [Int]
type PSet = S.Set Pos

parse :: Int -> String -> PSet
parse dimensions str = S.fromList [ x:y:(replicate (dimensions-2) 0) | (y,l) <- zip [0..] $ lines str, (x, c) <- zip [0..] l, c == '#']

neighbors :: Pos -> PSet
neighbors coords = S.fromList $ filter (coords /=) $ sequence $ map (\i -> [i-1,i,i+1]) coords

toCheck :: PSet -> PSet
toCheck = S.foldr (\p -> S.union (neighbors p)) S.empty

shouldBeActive :: PSet -> Pos -> Bool
shouldBeActive actives p
    | pIsActive && (numberActiveNeighbors == 2 || numberActiveNeighbors == 3) = True
    | (not pIsActive) && numberActiveNeighbors == 3 = True
    | otherwise = False
    where pIsActive = isActive p
          isActive x = S.member x actives
          numberActiveNeighbors = length $ S.filter isActive $ neighbors p

run :: PSet -> PSet
run p = S.filter (shouldBeActive p) $ toCheck p

main = do
    inp <- readFile "17.txt"
    print $ length $ (iterate run $ parse 3 inp) !! 6
    print $ length $ (iterate run $ parse 4 inp) !! 6