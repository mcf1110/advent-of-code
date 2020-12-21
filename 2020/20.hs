module Day20 where
import Data.List.Split (splitOn)
import qualified Data.IntMap as M

type TileMap = M.IntMap [String]

parse :: String -> (Int, [String])
parse str = (read tid, rots)
    where (tidStr:rs) = lines str
          [_, numberWithColon] = words tidStr
          tid = init numberWithColon
          rots = [head rs, map last rs, last rs, map head rs]


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

main = do
    inp <- splitOn "\n\n" <$> readFile "20.txt"
    let tiles = M.fromList $ parse <$> inp
    print $ product $ filter (isCornerTile tiles) $ M.keys tiles