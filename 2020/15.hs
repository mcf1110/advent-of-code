module Main where
import qualified Data.Map.Strict as M

type Id = Integer
type Number = Integer
data Turn = Turn {turnId :: Id, number :: Number, history :: M.Map Number Id} deriving Show

nextTurn (Turn tid n h) = Turn (tid+1) n' h'
    where h' = M.insert n tid h
          n' = maybe 0 (tid-) (M.lookup n h)
mkTurn ls = Turn (fromIntegral $ length ls) (last ls) (M.fromList $ zip (init ls) [1..])
getTurn n ls = number $ until ((==n) . turnId) nextTurn $ mkTurn ls

main = do
    inp <- readFile "15.txt"
    let ls = read $ "[" ++ inp ++ "]" -- quick n dirty ;)
    print $ getTurn 2020 ls
    print $ getTurn 30000000 ls -- overflows on ghci. please compile with -O2, runs in <1min