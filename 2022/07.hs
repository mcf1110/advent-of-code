{-# LANGUAGE PatternGuards #-}

module Day07 where

import Control.Monad.State.Strict
import Data.List

data Command = Cd Dir | Ls [Entry] deriving (Show, Eq)

data Dir = DirRoot | DirUp | DirName String deriving (Show, Eq)

data Entry = File {size :: Int, name :: String} | Folder {name :: String} deriving (Show, Eq)

parse :: [String] -> [Command]
parse [] = []
parse (x : xs)
  | Just dir <- stripPrefix "$ cd " x =
      Cd (parseDir dir) : parse xs
parse ("$ ls" : xs) =
  Ls (map parseEntry contents) : parse rest
  where
    (contents, rest) = break (isPrefixOf "$") xs

parseDir :: String -> Dir
parseDir "/" = DirRoot
parseDir ".." = DirUp
parseDir x = DirName x

parseEntry :: String -> Entry
parseEntry x | Just folder <- stripPrefix "dir " x = Folder folder
parseEntry x | [sizeStr, name] <- words x = File (read sizeStr) name

data Tree a = Terminal a | Node a [Tree a] deriving (Show)

data St = MkSt {tree :: Tree Entry, wd :: [String]}

fromEntry :: Entry -> Tree Entry
fromEntry f@(File {}) = Terminal f
fromEntry f@(Folder {}) = Node f []

buildTree :: [Command] -> Tree Entry
buildTree cmds = tree $ execState go initialState
  where
    initialState = MkSt (Node (Folder "/") []) []
    go = forM_ cmds $ \cmd -> do
      case cmd of
        Cd DirRoot -> modify (\s -> s {wd = []})
        Cd DirUp -> modify (\s -> s {wd = init $ wd s})
        Cd (DirName name) -> modify (\s -> s {wd = wd s <> [name]})
        Ls es -> forM_ es $ \entry -> modify (\s -> s {tree = create (wd s) entry (tree s)})

      return ()

create :: [String] -> Entry -> Tree Entry -> Tree Entry
create [] newEntry (Node f children) = Node f (children <> [fromEntry newEntry])
create (cd : ds) newEntry (Node f children) = Node f (createAt children)
  where
    createAt (node@(Node (Folder x) _) : rest)
      | x == cd = create ds newEntry node : rest
    createAt (node : rest) = node : createAt rest

isNode :: Tree a -> Bool
isNode (Node {}) = True
isNode _ = False

totalSizes :: Tree Entry -> ([Int], [Int])
totalSizes (Terminal (File {size})) = ([], [size])
totalSizes (Node (Folder x) ch) = (sum fileSizes : dirSizes, fileSizes)
  where
    (dirSizes, fileSizes) = foldMap totalSizes ch

needToFree :: [Int] -> Int
needToFree sizes = maximum sizes - 40_000_000

main :: IO ()
main = do
  input <- lines <$> readFile "07.txt"
  let sizes = fst $ totalSizes $ buildTree $ parse input
  print $ sum $ filter (<= 100000) sizes
  print $ minimum $ filter (>= needToFree sizes) sizes
