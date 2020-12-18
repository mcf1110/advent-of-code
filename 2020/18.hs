module Day18 where

-- GIVEN: ALL NUMBERS ARE SINGLE DIGITS
data Expr = Noop | Val Int | Sum | Mult | Nest [Expr] deriving (Show, Eq)

parse :: String -> (Expr, String)
parse ('(':cs) = let (n,cnt) = parseNest cs in (Nest n, cnt)
parse (')':cs) = (Noop, cs)
parse ('+':cs) = (Sum, cs)
parse ('*':cs) = (Mult, cs)
parse (c:cs) = (Val $ read [c], cs)

parseNest :: String -> ([Expr], String)
parseNest str = case parse str of
    (Noop, cnt) -> ([], cnt)
    (e, []) -> ([e], "")
    (e, cnt) -> (\(e1, c1) -> (e:e1, c1)) $ parseNest cnt

runParse :: String -> [Expr]
runParse = fst . parseNest

unparse :: Expr -> String
unparse Noop = ""
unparse Sum = " + "
unparse Mult = " * "
unparse (Val n) = show n
unparse (Nest e) = "(" <> mconcat (unparse <$> e) <> ")"

val :: Expr -> Int
val (Val i) = i
val (Nest es) = runExp es

-- part1
runExp :: [Expr] -> Int
runExp [x] = val x
runExp (x:o:y:rs) = runExp $ v:rs
    where v = Val $ f (val x) (val y)
          f = case o of
            Sum -> (+)
            Mult -> (*)

-- part2 (couldn't remeber how to combine parsers, so...)
data Tree = TVal Int | TSum Tree Tree | TMult Tree Tree deriving Show
type Parser = [Expr] -> Maybe (Tree, [Expr])
parseVal :: Parser
parseVal (Val i : xs) = Just (TVal i, xs)
parseVal (Nest es : xs) = case parseMultOrSumOrVal es of
    Just (t, xs') -> Just (t, xs' <> xs)
    r -> r
parseVal _ = Nothing

parseSumOrVal :: Parser
parseSumOrVal v = case parseVal v of
    Just (t1, (Sum:xs)) -> case parseSumOrVal xs of
        Just (t2, xs') -> Just $ (TSum t1 t2, xs')
        Nothing -> Nothing
    r -> r

parseMultOrSumOrVal :: Parser
parseMultOrSumOrVal v = case parseSumOrVal v of
    Just (t1, (Mult : xs)) -> case parseMultOrSumOrVal xs of
        Just (t2, xs') -> Just (TMult t1 t2, xs')
        Nothing -> Nothing
    r -> r

solve :: Tree -> Int
solve (TSum a b) = solve a + solve b
solve (TMult a b) = solve a * solve b
solve (TVal a) = a

part2 = fmap (solve . fst) . parseMultOrSumOrVal

main = do
    inp <- (map (runParse . filter (/= ' ')) . lines) <$> readFile "18.txt"
    print $ sum $ runExp <$> inp
    print $ sum $ (maybe 0 id . part2) <$> inp