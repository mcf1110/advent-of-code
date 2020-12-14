module Day14 where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

type Mask = M.Map Int Bool
data Code = SetMask Mask | SetMem Int Int deriving Show
type Memory = M.Map Int Int

parse1 :: String -> Code
parse1 ('m':'a':'s':'k':' ':'=':' ':mask) = parseMask
    where parseMask = SetMask $ M.fromList $ 
                        map (fmap (=='1')) $ 
                        filter ((/='X').snd) $ 
                        zip [0..] $ mask
parse1 ('m':'e':'m':'[':ps) = parseMem
    where 
        (p, rs) = span (/=']') ps
        parseMem = SetMem (read p) (read $ drop 4 rs)
    
toBits  0 x = []
toBits sz 0 = take sz $ repeat False
toBits sz x =  if k == 0 
    then False : (toBits n x) 
    else True  : (toBits n (x - k*m))
    where n = sz - 1
          m = 2^n
          k = x `div` m

fromBits xs = sum $ zipWith (*) (map (2^) [0..]) $ map(\x-> if x then 1 else 0) $ reverse xs

part1 :: (Memory, Mask) -> [Code] -> Memory
part1 (me, _) []      = me
part1 (me, ma) (c:cs) = case c of
    SetMask ma' -> part1 (me, ma') cs
    SetMem p v -> part1 (applySetMem ma p v me, ma) cs

applySetMem :: Mask -> Int -> Int -> Memory -> Memory
applySetMem msk p v mem = M.insert p v' mem
    where
        vb = zip [0..] $ toBits 36 $ v
        masked = map (\(i, b) -> fromMaybe b $ M.lookup i msk) vb
        v' = fromBits masked

data FBit = One | Zero | Cross deriving (Show, Eq)
type Mask2 = M.Map Int FBit
data Code2 = SetMask2 Mask2 | SetMem2 Int Int deriving Show

parse2 :: String -> Code2
parse2 ('m':'a':'s':'k':' ':'=':' ':mask) = parseMask
    where parseMask = SetMask2 $ M.fromList $ 
                        map (fmap (toMask)) $ 
                        zip [0..] $ mask
          toMask '1' = One
          toMask '0' = Zero
          toMask 'X' = Cross
parse2 ('m':'e':'m':'[':ps) = parseMem
    where 
        (p, rs) = span (/=']') ps
        parseMem = SetMem2 (read p) (read $ drop 4 rs)

part2 :: (Memory, Mask2) -> [Code2] -> Memory
part2 (me, _) []      = me
part2 (me, ma) (c:cs) = case c of
    SetMask2 ma' -> part2 (me, ma') cs
    SetMem2 p v -> part2 (applySetMem2 ma p v me, ma) cs

applySetMem2 :: Mask2 -> Int -> Int -> Memory -> Memory
applySetMem2 msk p v mem = M.union tmpMap mem
    where
        addr = getAdresses msk $ toBits 36 p
        tmpMap = M.fromList $ map (\k -> (k,v)) addr


pp xs = map (\b -> if b then '1' else '0') xs

getAdresses :: Mask2 -> [Bool] -> [Int]
getAdresses msk bs = possibilites
    where ones = M.map (const True) $ M.filter (==One) msk
          floatings = M.keysSet $ M.filter (==Cross) msk
          onesMasked = map (\(i, b) -> fromMaybe b $ M.lookup i ones) $ zip [0..] bs
          possibilites = map (fromBits . reverse) $ go [[]] $ zip [0..] onesMasked
          go acc [] = acc
          go acc ((i, x):xs)
            | S.member i floatings = (go (map (False:) acc) xs) ++ (go (map (True:) acc) xs)
            | otherwise = go (map (x:) acc) xs

main = do
    inp <- lines <$> readFile "14.txt"
    let mem = M.empty
        mask = M.empty
    print $ sum $ M.elems $ part1 (mem, mask) $ map parse1 inp
    print $ sum $ M.elems $ part2 (mem, mask) $ map parse2 inp