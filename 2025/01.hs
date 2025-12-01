
module Day01 where


type Rotation = Int

parse :: String -> [Rotation]
parse = map aux . lines
    where 
        aux :: String -> Rotation
        aux ('L':xs) = -1 * read xs
        aux ('R':xs) = read xs

part1 :: [Rotation] -> Int
part1 = length . filter (==0) . scanl (\acc rot -> (acc + rot) `mod` 100) 50


part2 :: [Rotation] -> Int
part2 rs = length $ filter (==0) cs
    where 
        cs = scanl (\pos rot -> (pos + rot) `mod` 100) 50 ones
        ones = concatMap (\r -> replicate (abs r) (signum r)) rs

main :: IO ()
main = do
  x <- parse <$> readFile "01.txt"
  print $ part1 x
  print $ part2 x