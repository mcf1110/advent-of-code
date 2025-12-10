module Day06 where

import Data.List

parse1 :: String -> [([Int], String)]
parse1 = map parseCol . transpose . map words . lines
    where
        parseCol xs = (map read $ init xs, last xs)


parse2 :: String -> [([Int], String)]
parse2 = go [] . reverse . transpose . lines
    where
        go is (x:xs) = 
            let i = init x
                l = last x in
            case words i of
                [] -> go is xs --skip
                [n] -> case l of
                    ' ' ->go (read n:is) xs
                    op -> (read n:is, op:[]):(go [] xs)
        go _ [] = []

calculate :: [([Int], String)] -> Int
calculate = sum . map solve
    where
        solve (is, "+") = sum is
        solve (is, "*") = product is
        solve _ = undefined

main :: IO ()
main = do
  txt <- readFile "06.txt"
  print $ calculate $ parse1 txt
  print $ calculate $ parse2 txt