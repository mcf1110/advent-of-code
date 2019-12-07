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

  part1 :: OrbitMap -> Int
  part1 os = sum $ distance os <$> os

  pathToCom :: OrbitMap -> String -> [String]
  pathToCom om "COM" = ["COM"]
  pathToCom om x = x:(pathToCom om $ om M.! x)

  part2 :: OrbitMap -> Int
  part2 os = ((length you) + (length santa) - 2 * (length diff)) - 2
    where 
      (you, santa) = (reverse $ pathToCom os "YOU", reverse $ pathToCom os "SAN")
      diff = takeWhile (flip (elem) santa) you

  main = do
    text <- readFile "./Day6.txt"
    print $ part2 $ parse text
