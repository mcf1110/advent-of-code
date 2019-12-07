module Day6 where
  import Data.Maybe (catMaybes)
  import Data.List

  parse :: String -> [Orbit]
  parse str = 
    (\(a, _:b) -> (a,b)) <$> 
    span (/= ')') <$> 
    lines str

  type Orbit = (String, String)

  t = [("COM","B"),("B","C"), ("C", "D")]

  look :: [Orbit] -> String -> Orbit
  look [] _ = error "No orbit"
  look (o@(_,s):xs) str
    | str == s = o
    | otherwise = look xs str

  distance :: [Orbit] -> Orbit -> Int
  distance _ ("COM", _) = 1
  distance os (a, b) = 1 + (distance os $ look os a)

  solve :: [Orbit] -> Int
  solve os = sum $ distance os <$> os

  main = do
    text <- readFile "./Day6.txt"
    print $ solve $ parse text
