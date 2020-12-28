module Main where
import Data.List (findIndex)

loops subject = iterate f 1
    where f v = (v*subject) `mod` 20201227

main = do
    [card, door] <- (map read) <$> lines <$> readFile "25.txt" :: IO [Int]
    let ls = loops 7
    let cardLoop = findIndex (==card) ls
    -- let doorLoop = findIndex (==door) ls -- huh, wasn't needed after all
    let encriptionKey = ((loops door) !!) <$> cardLoop
    print $ encriptionKey