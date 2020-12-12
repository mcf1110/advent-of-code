module Day12 where

parse :: [Char] -> (Char, Integer)
parse (c:ns) = (c, read ns)
parse _ = error "Vish"

turn :: Integer -> (Integer, Integer, Char) -> (Integer, Integer, Char)
turn amt (x, y, fc) = (x, y, dirs !! (fromIntegral n))
    where
        n = amt `div` 90
        dirs = dropWhile (/= fc) $ cycle ['N', 'E', 'S', 'W']

move :: (Char, Integer) -> (Integer, Integer, Char) -> (Integer, Integer, Char)
move (dir, amt) (x,y,fc) = case dir of
    'E' -> (x+amt, y, fc)
    'W' -> (x-amt, y, fc)
    'N' -> (x, y+amt, fc)
    'S' -> (x, y-amt, fc)

step1 :: (Char, Integer) -> (Integer, Integer, Char) ->  (Integer, Integer, Char)
step1 ('R', amt) st          = turn (amt) st
step1 ('L', amt) st          = turn (360-amt) st
step1 ('F', amt) st@(_,_,fc) = move (fc, amt) st
step1 mv         st          = move mv st

rotateWP :: Integer -> (Integer, Integer) -> (Integer, Integer)
rotateWP amt wp = (iterate (\(x,y) -> (y,-x)) wp) !! (fromIntegral n)
    where n = amt `div` 90

step2 :: (Char, Integer) -> ((Integer, Integer), (Integer, Integer)) ->  ((Integer, Integer), (Integer, Integer))
step2 ('R', amt) (sp, wp)             = (sp, (rotateWP amt wp))
step2 ('L', amt) (sp, wp)             = (sp, (rotateWP (360-amt) wp))
step2 ('F', amt) ((sx, sy), (wx, wy)) = ((sx+wx*amt, sy+wy*amt),(wx,wy))
step2 mv         (sp,       (wx, wy)) = (sp, dropThird $ move mv (wx, wy, ' '))

dropThird (a,b,_) = (a,b)

main = do
    inp <- (map parse) . lines <$> readFile "12.txt"
    let run s = foldl1 (flip (.)) $ s <$> inp
        manhattan (x,y) = abs x + abs y
    print $ manhattan . dropThird $ run step1 (0,0,'E')
    print $ manhattan . fst $ run step2 $ ((0,0), (10,1))
