module Day07 where

type Position = Int
type Fuel = Int

main :: IO ()
main = do
  input <- readFile "07.txt"
  let positions = (read $ "["<>input<>"]" :: [Position])
      allPositions = [minimum positions..maximum positions]
  print $ minimum $ testPosition part1 positions <$> allPositions
  print $ minimum $ testPosition part2 positions <$> allPositions

testPosition :: (Position -> Position -> Fuel) -> [Position] -> Position -> Fuel
testPosition f ps p = sum $ map (f p) ps

part1 :: Position -> Position -> Fuel
part1 desired current = abs $ current - desired

part2 :: Position -> Position -> Fuel
part2 desired current = n*(1+n) `div` 2 -- sum [1..n], but much faster!
    where n = part1 current desired