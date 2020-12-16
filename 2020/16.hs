module Day16 where
import Data.List.Split
import Data.List (transpose, isPrefixOf)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Field = Field {name :: String, r1 :: (Int, Int), r2 :: (Int, Int)} deriving Show
type Input = ([Field], [Int], [[Int]])

parse :: [String] -> Input
parse [f, y, n] = (fields, your, nearby)
    where rl str = read $ "[" ++ str ++ "]" :: [Int]
          your   = rl $ (lines y) !! 1
          nearby = map rl $ tail $ lines n
          fields = parseField <$> lines f
parse _ = error "Parse error"
parseField field = Field nm (mkRng str1) (mkRng str2)
    where [nm, rngs]   = splitOn ": " field
          [str1, str2] = splitOn " or " rngs
          mkRng s      = (\[a,b] -> (a,b)) $ map read $ splitOn "-" s

isField :: Int -> Field -> Bool
isField n f = between (r1 f) || between (r2 f)
    where between (a,b) = n >= a && n <= b

isValid :: [Field] -> Int -> Bool
isValid fs n = any (isField n) fs

part1 :: Input -> Int
part1 (fs, _, nby) = sum $ nby >>= (filter $ not . isValid fs)

discardInvalidTickets :: Input -> Input
discardInvalidTickets (f,y,nby) = (f,y,nby')
    where nby' = filter (all $ isValid f) nby

possibilities :: Input -> [S.Set String]
possibilities (fs, y, n) = map (S.fromList . possibleFields) $ transpose (y:n)
    where possibleFields ns = map name $ filter (isPossible ns) fs
          isPossible ns f = all (\i -> isField i f) ns

solve :: Input -> M.Map String Int
solve inp = go M.empty p where
    p = possibilities inp
    l = length p
    go m ps
        | length ans == l = ans
        | otherwise       = go ans ps'
        where
            ans = M.union m' m
            m' = M.fromList $ map (\(s,i) -> (S.elemAt 0 s, i)) $ filter ((==1). length . fst) $ zip ps [0..]
            ks = M.keysSet m'
            ps' = map (S.\\ ks) ps

part2 :: Input -> Int
part2 inp = product $ map (y!!) deps
    where deps = M.elems $ M.filterWithKey (\k _ -> isPrefixOf "departure" k) $ solve inp
          (_,y,_) = inp

main = do
    inp <- parse <$> splitOn "\n\n" <$> readFile "16.txt"
    let valid = discardInvalidTickets inp
    print $ part1 inp
    print $ part2 valid