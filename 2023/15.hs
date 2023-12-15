{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Data.Text qualified as T
import Data.Text.Read (decimal)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

hash :: T.Text -> Int
hash = go 0 . T.unpack
  where
    go :: Int -> String -> Int
    go acc [] = acc
    go acc (x : xs) = go (17 * (acc + fromEnum x) `mod` 256) xs

data Step = SetLens T.Text Int | DecLens T.Text deriving (Show)

getKey :: Step -> Int
getKey (SetLens l _) = hash l
getKey (DecLens l) = hash l

part2 :: [T.Text] -> V.Vector [(T.Text, Int)]
part2 steps = foldl (\v s -> V.modify (\v' -> MV.modify v' (runStep s) (getKey s)) v) (V.replicate 256 []) (map parse steps)
  where
    parse :: T.Text -> Step
    parse t
      | T.last t == '-' = DecLens $ T.init t
      | otherwise = (\(l, i) -> SetLens l (read $ T.unpack $ T.tail i)) $ T.breakOn "=" t

runStep :: Step -> [(T.Text, Int)] -> [(T.Text, Int)]
runStep (SetLens l n) [] = [(l, n)]
runStep (SetLens l n) (x : xs)
  | l == fst x = (l, n) : xs
  | otherwise = x : runStep (SetLens l n) xs
runStep (DecLens l) [] = []
runStep (DecLens l) (x : xs)
  | l == fst x = xs
  | otherwise = x : runStep (DecLens l) xs

focusingPower :: V.Vector [(T.Text, Int)] -> Int
focusingPower v = sum $ zipWith (\i xs -> sum $ map (* i) xs) [1 ..] $ map (zipWith (*) [1 ..] . map snd) (V.toList v)

main :: IO ()
main = do
  input <- T.splitOn "," . T.pack <$> readFile "./15.txt"
  print $ sum $ map hash input
  print $ focusingPower $ part2 input