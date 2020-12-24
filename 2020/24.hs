module Day24 where
import qualified Data.Set as S

data Dir = E | SE | SW | W | NW | NE deriving (Show, Enum)
type Coord = (Int, Int)

parse :: String -> [Dir]
parse [] = []
parse ('e':rs) = E:(parse rs)
parse ('w':rs) = W:(parse rs)
parse ('s':'e':rs) = SE:(parse rs)
parse ('s':'w':rs) = SW:(parse rs)
parse ('n':'e':rs) = NE:(parse rs)
parse ('n':'w':rs) = NW:(parse rs)
parse _ = error "No parse"

move :: Dir -> Coord -> Coord
move E (x,y) = (x+1, y)
move W (x,y) = (x-1, y)
move SW (x,y) = (if even y then x-1 else x, y+1)
move SE (x,y) = (if even y then x else x+1, y+1)
move NE (x,y) = (if even y then x else x+1, y-1)
move NW (x,y) = (if even y then x-1 else x, y-1)

toggle :: Ord a => a -> S.Set a -> S.Set a
toggle x xs = if S.member x xs then (S.delete x xs) else (S.insert x xs)

adjacent :: Coord -> S.Set Coord
adjacent c = S.fromList $ (`move` c) <$> [E .. NE]

toCheck :: S.Set Coord -> S.Set Coord
toCheck = foldr1 (S.union) . S.map adjacent

conway :: S.Set Coord -> S.Set Coord
conway cs = S.filter shouldBeBlack candidates
    where candidates = toCheck cs
          nBlacks c = S.size $ S.intersection cs $ adjacent c
          shouldBeBlack c
            | S.member c cs = (nBlacks c) `elem` [1,2]
            | otherwise = nBlacks c == 2

main = do
    inp <- (map parse) . lines <$> readFile "24.txt"
    let coords = foldr move (0,0) <$> inp
    let blacks = foldr toggle S.empty coords
    print $ S.size $ blacks
    print $ S.size $ (iterate conway blacks) !! 100