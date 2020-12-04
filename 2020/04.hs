module Day04 where

import Data.List.Split
import Data.Maybe
import Data.List
import Data.Char

-- Yep, that's a pun.
parseport :: String -> [(String, String)]
parseport str = do
    str' <- endByOneOf " \n" str
    let [k, v] = splitOn ":" str'
    return $ (k, v)

isValid1 :: [(String, String)] -> Bool
isValid1 pass = contains ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] $ map fst pass
    where contains req test = all (\r -> elem r test) req

isValid2 :: [(String, String)] -> Bool
isValid2 pass = isValid1 pass && all valid pass
    where 
        between lo hi str = let i = read str in i >= lo && i <= hi
        valid ("byr", v) = between 1920 2002 v
        valid ("iyr", v) = between 2010 2020 v
        valid ("eyr", v) = between 2020 2030 v
        valid ("hgt", v) = valHeight $ split (whenElt (not . isDigit)) v
        valid ("hcl", ('#':v)) = length v == 6 && all (\x -> isDigit x || x `elem` ['a'..'f']) v
        valid ("ecl", v) = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        valid ("pid", v) = length v == 9 && all isDigit v
        valid ("cid", v) = True
        valid _ = False

        valHeight (h:(u:_):_) = if u == 'c' then between 150 193 h else between 59 76 h
        valHeight _ = False


s :: [Bool] -> Int
s bs = sum $ map (\x -> if x then 1 else 0) bs

main = do
    inp <- map parseport <$> splitOn "\n\n" <$> readFile "04.txt"
    print $ s $ map isValid1 inp
    print $ s $ map isValid2 inp
    