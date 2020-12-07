module Day07 where
import Data.List
import Data.Map.Strict
import qualified Data.Matrix as M
import qualified Data.Vector as V

type Color = String

colors ("no":"other":('b':'a':'g':_):rs) = colors rs
colors (x:y:('b':'a':'g':_):rs) = (x <> y):(colors rs)
colors (_:rs) = colors rs
colors [] = []

cMap :: [String] -> Map Color Int
cMap lines = fromList $ zip cs [1..]
    where cs = nub $ concat $ colors <$> words <$> lines

buildMatrix :: Map Color Int -> M.Matrix Int -> String -> M.Matrix Int
buildMatrix cm mx line = ret
    where
        (a:b:_:_:contents) = words line
        toColorCode :: String -> String -> Int
        toColorCode s1 s2 = cm ! (s1 <> s2)
        from = toColorCode a b
        ret = case contents of
            ("no":"other":_) -> mx
            x -> getCs mx x
        getCs m (qt:c1:c2:_:rs) = M.setElem (read qt) (from, toColorCode c1 c2) $ getCs m rs
        getCs m _ = m

findHowManyFit :: M.Matrix Int -> Int -> [Int]
findHowManyFit mx c = holders ++ (holders >>= findHowManyFit mx)
    where holders = fmap (+1) $ V.toList $ V.findIndices (>0) $ M.getCol c mx

getNBags :: M.Matrix Int -> Int -> Int
getNBags mx c = r + (V.sum $ V.map snd subbags)
    where 
        subbags = V.filter ((>0) . snd)$ V.indexed $ M.getRow c mx
        r = V.sum $ V.map (\(c, qt) -> qt * getNBags mx (c + 1)) subbags

main = do
    inp <- lines <$> readFile "07.txt"
    let cm = cMap inp
        s  = size cm
        mx = Data.List.foldl (buildMatrix cm) (M.zero s s) inp
        sg = cm ! ("shinygold")
    print $ length $ nub $ findHowManyFit mx sg
    print $ getNBags mx sg