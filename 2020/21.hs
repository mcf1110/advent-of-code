module Day21 where
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (partition, intercalate)

type StrSet = S.Set String
type Ingrs = StrSet
type Alls  = StrSet

parse :: String -> (Ingrs, Alls)
parse str = (S.fromList $ words is, as')
    where (is, as) = break (=='(') str
          as' = S.fromList $ splitOn ", " $ init $ drop 10 as

candidates :: [(Ingrs, Alls)] -> String -> Ingrs
candidates isas a = foldl1 S.intersection $ fst <$> filter (S.member a . snd) isas

howManyTimesAppears :: [(Ingrs, Alls)] -> String -> Int
howManyTimesAppears isas a = length $  filter (S.member a . fst) isas

part2 :: M.Map String String -> [(String, StrSet)] -> String
part2 m allToIngrs = case notSure of
    [] -> intercalate "," $ M.elems $ m'
    _  -> part2 m' $ map (fmap (`S.difference` eSet)) notSure
    where (sure, notSure) = partition ((==1) . S.size . snd) allToIngrs
          m' = M.union m $ M.fromList $ map (fmap (head . S.toList)) sure
          eSet = S.fromList $  M.elems m'

main = do
    inp <-  map parse <$> lines <$> readFile "21.txt"
    let allergens    = S.toList $ foldl1 S.union $ map snd $ inp
        ingrSet      = foldl1 S.union $ map fst $ inp
        canBeAller   = foldl1 S.union $ candidates inp <$> allergens
        sureNotAller = S.toList $ S.difference ingrSet canBeAller
    print $ sum $ howManyTimesAppears inp <$> sureNotAller
    putStrLn $ part2 M.empty $ zip allergens $ candidates inp <$> allergens