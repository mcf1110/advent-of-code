module Day20 where
import Data.List.Split (splitOn, chunksOf)
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import qualified Data.List as L
import Data.Maybe (catMaybes )

type TileMap = M.IntMap [String]

parse :: String -> (Int, [String])
parse str = (read tid, rs)
    where (tidStr:rs) = lines str
          [_, numberWithColon] = words tidStr
          tid = init numberWithColon

getBorders :: [String] -> [String]
getBorders rs = [head rs, map last rs, last rs, map head rs]

flips :: [String] -> [String]
flips borders = borders >>= (\b -> [b, reverse b])

doesBorderFit :: [String] -> String -> Bool
doesBorderFit bs target = any (==target) bs

howManyFits :: TileMap -> Int -> Int
howManyFits tm k = length $ filter (id) $ map (doesBorderFit allRotated) borders
    where (Just borders, tm') = M.updateLookupWithKey (\_ _ -> Nothing) k tm
          allRotated = flips $ mconcat $ M.elems tm'
          
isCornerTile :: TileMap -> Int -> Bool
isCornerTile tm k = (howManyFits tm k) == 2

-- part2
rotate :: [String] -> [[String]]
rotate rs = [rs, L.transpose rs] >>= (\b -> [b, reverse b]) >>= (\b -> [b, map reverse b])

fits :: [String] -> [String] -> Bool
fits a b = head a == head b

fitTile :: S.IntSet -> TileMap -> [String] -> Maybe (S.IntSet, [String])
fitTile except tm tile = case ls of
        [] -> Nothing
        x -> let (tid, nextTile) = head x in 
                Just (S.insert tid except, (reverse $ nextTile) <> tile)
    where 
          tmWithoutExcepts = M.withoutKeys tm except
          rots = M.map rotate tmWithoutExcepts
          ls = M.toList $ M.map head $ M.filter (not.null) $ M.map (filter (fits tile)) rots


makeRow except tile tm = case catMaybes $ fitTile except tm <$> rotate tile of
                            [] -> (except, tile)
                            ((ex', t):_) -> makeRow ex' t tm

makeFirstRow :: [Int] -> M.IntMap [String] -> (S.IntSet, [String])
makeFirstRow corners tm = makeRow (S.singleton c1) ((M.!) tm c1) tm
    where c1 = head corners

makeCols :: TileMap -> (S.IntSet, [[[String]]]) -> [String] -> (S.IntSet, [[[String]]])
makeCols tm (e, cols) tile = (S.union e e', sliced:cols)
    where (e', newCol) = makeRow e tile tm
          sliced = sliceTiles newCol

sliceTiles :: [String] -> [[String]]
sliceTiles ts = chunksOf n ts
    where n = length $ head ts

mergeV [x] = x
mergeV (x:y:rs) = mergeV $ ((init x) <> (tail y)):rs

mergeH [x] = x
mergeH (x:y:rs) = mergeH $ (zipWith (\a b -> (init a) <> (tail b)) x y):rs

removeBorders = init . tail . map (init . tail)

isMonster mt = all (=='#') elems
    where offsets = mconcat $ zipWith (\r cs -> [(r,c) | c <- cs]) [0..] [[18], [0,5,6,11,12,17,18,19], [1,4,7,10,13,16]]
          elems = map (\(r,c) -> mt !! r !! c) offsets

nMonsters mt = length $ filter isMonster [subm r c | r <- [0..(rows-2)], c <- [0..(cols-19)]]
    where rows = length mt
          cols = length $ head mt
          subm r c = take 3 $ map (take 20 . drop c) $ drop r mt
main = do
    inp <- splitOn "\n\n" <$> readFile "20.txt"
    let tiles = M.fromList $ parse <$> inp
        borders = M.map getBorders tiles
        corners = filter (isCornerTile borders) $ M.keys borders
        (e, rs) = sliceTiles <$> makeFirstRow corners tiles
        (_, cols) = foldl (makeCols tiles) (e, []) rs
        pic = removeBorders $ mergeH $ reverse (mergeV <$> cols)
        pics = rotate pic
        actualNMonster = maximum $ nMonsters <$> pics
        nHashtags = length $ filter(=='#') $ mconcat pic
    print $ product corners
    print $ nHashtags  - (15 * actualNMonster)