module Day6 where
  import Data.Maybe (catMaybes)
  import qualified Data.Map as M
  import Data.List

  parse :: String -> OrbitMap
  parse str = M.fromList $
    (\(a, _:b) -> (b,a)) <$> 
    span (/= ')') <$> 
    lines str

  type OrbitMap = M.Map String String

  t = [("COM","B"),("B","C"), ("C", "D")]

  distance :: OrbitMap -> String -> Int
  distance _ "COM" = 1
  distance o a = 1 + (distance o $ o M.! a)

  solve :: OrbitMap -> Int
  solve os = sum $ distance os <$> os

  main = do
    text <- readFile "./Day6.txt"
    print $ solve $ parse text
