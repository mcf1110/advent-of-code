module Day08 where

import Data.List (transpose)
import Data.Map qualified as M
import Data.Vector qualified as V

data LeftRight = L | R deriving (Show, Eq, Read)

parseLRs :: String -> V.Vector LeftRight
parseLRs = V.fromList . map (read . (: []))

-- AAA = (BBB, CCC)
parseNodes :: [String] -> M.Map String (String, String)
parseNodes = M.fromList . map parseNode
  where
    parseNode :: String -> (String, (String, String))
    parseNode s = (name, (left, right))
      where
        name = take 3 s
        left = take 3 $ drop 7 s
        right = take 3 $ drop 12 s

navigate :: M.Map String (String, String) -> V.Vector LeftRight -> String -> [String]
navigate nodes lrs from = go (from, 0)
  where
    go :: (String, Int) -> [String]
    go (name, index) = name : go (nextName, nextIndex)
      where
        (left, right) = nodes M.! name
        nextIndex = (index + 1) `mod` length lrs
        nextName = case lrs V.! index of
          L -> left
          R -> right

main :: IO ()
main = do
  input <- lines <$> readFile "08.txt"
  let nodes = parseNodes $ drop 2 input
      lrs = parseLRs $ head input
      navFrom = navigate nodes lrs
      endsWith x = (== x) . last

      starts = filter (endsWith 'A') $ M.keys nodes

  print $ length $ takeWhile (/= "ZZZ") $ navFrom "AAA"
  print $ foldr1 lcm $ map (length . takeWhile (not . endsWith 'Z') . navFrom) starts
