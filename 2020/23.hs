module Day23 where
import Data.List
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Debug.Trace
import Data.Array.Base (UArray)
import qualified Data.Array.IArray as A

tgt :: Int -> Int -> Int -> Int -> Int -> Int
tgt size cur p1 p2 p3 = head $ filter (not . (`elem` [p1,p2,p3])) $ filter (>=1) $ ([cur-1, cur-2, cur-3] <> [size,size-1..])

run :: Int -> [Int] -> UArray Int Int
run rounds inp = runSTUArray $ do
    let size = length inp
    let start = head inp :: Int
    as <- newArray_ (1, size) :: ST s (STUArray s Int Int)
    forM_ (zip (inp) (tail inp <> [head inp])) (\(c,n) -> writeArray as c n)

    foldM_ (\cur _ -> do
            p1 <- readArray as cur
            p2 <- readArray as p1
            p3 <- readArray as p2
            p4 <- readArray as p3
            next <- return $ tgt size cur p1 p2 p3
            rightFromNext <- readArray as next
            writeArray as cur p4
            writeArray as next p1
            writeArray as p3 rightFromNext
            return p4
        ) start [1..rounds]
    return as

part1 :: [Int] -> String
part1 inp = concat $ map show $ unfoldr f $ a A.! 1
    where a = run 100 inp
          f :: Int -> Maybe (Int, Int)
          f 1 = Nothing
          f i =Just (i, a A.! i)


million = 1000 * 1000

part2 :: [Int] -> Int
part2 inp = c1*c2
    where inp' = let m = maximum inp + 1 in inp <> [m..million]
          a = run (10*million) inp'
          c1 = a A.! 1
          c2 = a A.! c1

main = do
    inp <- map (read . (:[])) <$> readFile "23.txt"
    putStrLn $ part1 inp
    print $ part2 inp