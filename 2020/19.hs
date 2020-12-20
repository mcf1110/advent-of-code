module Day19 where
import qualified Data.IntMap.Strict as M
import Data.List.Split (splitOn)
import Control.Monad

type RuleId = Int
data Rule = Seq [RuleId] | Option [RuleId] [RuleId] | Literal Char deriving Show

parse str = (read id, parsed)
    where [id, rs] = splitOn ": " str
          splitted = splitOn "|" rs
          toRuleIdList str = map read $ words str
          parsed
            | head rs == '"' = Literal $ rs !! 1
            | length splitted == 1 = Seq $ toRuleIdList rs
            | otherwise = let [a,b] = splitted in Option (toRuleIdList a) (toRuleIdList b)

verify :: M.IntMap Rule -> RuleId -> String -> [String]
verify rules id str = case rules M.! id of
    Literal a -> if take 1 str == [a] then [tail str] else []
    Seq ids -> verifyIds ids str
    Option as bs -> verifyIds as str <> verifyIds bs str
    where verifyIds ids str = foldl1 (>=>) (map (verify rules) ids) str

part1 rules msgs = length $ filter id $ map (any (== "")) $ verify rules 0 <$> msgs
part2 rules msgs = part1 rules' msgs
    where newRules = M.fromList [(8, Option [42] [42, 8]), (11, Option [42,31] [42,11,31])]
          rules' = M.union newRules rules


main = do
    [rs, ms] <- splitOn "\n\n" <$> readFile "19.txt"
    let rules = M.fromList $ parse <$> lines rs
        messages = lines ms
    print $ part1 rules messages
    print $ part2 rules messages