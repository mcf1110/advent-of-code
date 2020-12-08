module Day08 where
import Data.Maybe (mapMaybe)

data Op = Nop Int | Acc Int | Jmp Int | Halt | Error deriving Show
data Zipper a = Zipper {previous:: [a], focus :: a, next :: [a]} deriving Show
type State = (Zipper Op, Int)

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs
toList :: Zipper a -> [a]
toList (Zipper p f n) = reverse p ++ f:n

jmp :: Int -> Zipper a -> Zipper a
jmp 0 z = z
jmp x (Zipper p f n)
    | x > 0     = jmp (x-1) $ Zipper (f:p) (head n) (tail n)
    | otherwise = jmp (x+1) $ Zipper (tail p) (head p) (f:n)

start :: Zipper a -> Zipper a
start (Zipper (p:ps) f n) = start $ Zipper ps p (f:n)
start z = z

setFocus :: Zipper a -> a -> Zipper a
setFocus (Zipper p _ n) x = Zipper p x n

r ('+':v) = read v
r ('-':v) = - (read v)

parse ('n':'o':'p':v) = Nop (r $ tail v)
parse ('a':'c':'c':v) = Acc (r $ tail v)
parse ('j':'m':'p':v) = Jmp (r $ tail v)

run1 :: State -> State
run1 (z, i) = case focus z of
    Halt -> (z, i)
    Nop _ -> run1 (jmp 1 z', i)
    Jmp x -> run1 (jmp x z', i)
    Acc x -> run1 (jmp 1 z', i + x)
    where z' = setFocus z Halt

run2 (z, i) = case focus z of
    Error -> Nothing
    Halt -> Just (z, i)
    Nop _ -> run2 (jmp 1 z', i)
    Jmp x -> run2 (jmp x z', i)
    Acc x -> run2 (jmp 1 z', i + x)
    where z' = setFocus z Error

pgms :: Zipper Op -> [Zipper Op]
pgms z = case focus z of
    Halt  -> []
    Acc _ -> nxt
    Nop x -> (start $ setFocus z (Jmp x)):nxt
    Jmp x -> (start $ setFocus z (Nop x)):nxt
    where nxt = pgms $ jmp 1 z

main = do
    inp <- lines <$> readFile "08.txt"
    let pgm = (parse <$> inp) ++ [Halt]
        st = (fromList $ pgm, 0)
    print $ snd $ run1 st
    print $ snd . head $ mapMaybe (\p -> run2 (p, 0)) $ pgms $ fromList pgm