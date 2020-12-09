module Day09 where
import Control.Comonad
import qualified Data.List.NonEmpty as L
import Data.List (intersect)

preambleSize = 25

isInvalid (n:pre) = null $ intersect shouldExist pre
    where shouldExist = (`subtract` n) <$> pre

findGroup target list = go list 2
    where go xs n
            | n < 2       = go xs (n+1)
            | s == target = take n xs
            | s > target  = go (tail xs) (n-1)
            | otherwise   = go xs (n+1)
            where s = sum $ take n xs

main = do
    inp <- (map read) . lines <$> readFile "09.txt"
    let s = length inp - preambleSize
        ns = take s $ L.toList $ extend (L.take (preambleSize+1)) $ L.fromList $ reverse inp
        firstInvalid = head $ head <$> filter isInvalid $ ns
        grp = findGroup firstInvalid inp
    print $ firstInvalid
    print $ maximum grp + minimum grp