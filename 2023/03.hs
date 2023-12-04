module Day03 where

import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe

data Entity a = Entity {value :: a, positions :: [(Int, Int)]} deriving (Show, Functor)

getNumbers :: Matrix Char -> [Entity Int]
getNumbers m = map convert $ concatMap (extract . foldl f ([], Nothing)) (toLists (mapPos const m))
  where
    f :: ([Entity String], Maybe (Entity String)) -> (Int, Int) -> ([Entity String], Maybe (Entity String))
    f (es, Nothing) pos
      | isDigit (m ! pos) = (es, Just (Entity [m ! pos] [pos]))
      | otherwise = (es, Nothing)
    f (es, Just (Entity xs ps)) pos
      | isDigit (m ! pos) = (es, Just (Entity ((m ! pos) : xs) (pos : ps)))
      | otherwise = (Entity xs ps : es, Nothing)

    extract :: ([Entity String], Maybe (Entity String)) -> [Entity String]
    extract (es, Nothing) = es
    extract (es, Just e) = e : es
    convert :: Entity String -> Entity Int
    convert (Entity xs ps) = Entity (read $ reverse xs) (sort ps)

expandPositions :: [(Int, Int)] -> [(Int, Int)]
expandPositions ps = nub $ do
  (x, y) <- ps
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1)]

isPartNumber :: Matrix Char -> Entity Int -> Bool
isPartNumber m (Entity v ps) = any isSymbol around
  where 
    expanded = expandPositions ps
    around = mapMaybe (\(i,j) -> safeGet i j m) expanded
    isSymbol ch = not (ch == '.' || isDigit ch)

getStars :: Matrix Char -> [Entity ()]
getStars = 
  catMaybes . 
  toList . 
  mapPos (\(i, j) ch -> if ch == '*' then Just(Entity () [(i, j)]) else Nothing)

getGearRatios :: [Entity Int] ->  [Entity ()] ->[Int]
getGearRatios numbers = mapMaybe getGearRatio
  where
    getGearRatio s = if length adjacentNumbers == 2 then Just $ product adjacentNumbers else Nothing
      where 
        ps = expandPositions $ positions s
        adjacentNumbers = map value $ filter (\n -> not $ null $ positions n `intersect` ps) numbers
      

main :: IO ()
main = do
  input <- fromLists . lines <$> readFile "03.txt"
  let partNumbers = filter (isPartNumber input) $ getNumbers input
  print $ sum $ map value partNumbers
  print $ sum $ getGearRatios partNumbers $ getStars input
