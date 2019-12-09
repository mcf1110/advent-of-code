module Day07 where
  import IntCode as IC
  import Data.List(permutations)

  import Data.Sequence as S

  runAmps :: String -> [Int] -> Int
  runAmps pg modes = go modes [0]
    where
      go [] (input:_) = input
      go (m:ms) (input:_) = go ms $ run pg [m, input]
  
  findMax :: String -> Int
  findMax pg = maximum $ fmap (runAmps pg) $ permutations $ [0..4]

  runUntilOutput :: IC.ProgramState -> Maybe (IC.ProgramState, Int)
  runUntilOutput pg = case pg' of
    Just pgSt'@(_, _, []) -> runUntilOutput pgSt'
    Just pgSt'@(a, b, out:outs) -> Just $ ((a, b, outs), out)
    Nothing -> Nothing
    where pg' = IC.runIteration pg
  
  modesToInputs :: [Int] -> S.Seq [Int]
  modesToInputs ms = addToInput 0 0 $ fromList((:[]) <$> ms)
    where addToInput val = S.adjust' (\ls -> ls ++ [val]) 
  
  runFeedback :: (Int, S.Seq IC.ProgramState) -> Int -> Int
  runFeedback (i, pgs) lastE = case (runUntilOutput $ index pgs i, i) of
    (Nothing, 4) -> lastE
    (Nothing, _) -> runFeedback (nextIndex, pgs) lastE
    (Just (current', out), 4) -> runFeedback (getNextIter current' out) out
    (Just (current', out), _) -> runFeedback (getNextIter current' out) lastE
    where 
      nextIndex = (i + 1) `mod` (S.length pgs)
      getNextIter current' out = (nextIndex, newStates)
        where
          (nextPg, nextI, nextO) = index pgs nextIndex
          nextI' = nextI ++ [out]
          newNext = (nextPg, nextI', nextO)
          newStates = update nextIndex newNext $ update i current' $ pgs

  
  feedback :: String -> [Int] -> Int
  feedback str modes = runFeedback (0, programs) 0
    where
      initMemory = IC.strToMemory str
      inputs = modesToInputs modes
      programs = fmap (\i -> (initMemory, i, [])) inputs
  
  findMaxFeedback :: String -> Int
  findMaxFeedback pg = maximum $ fmap (feedback pg) $ permutations $ [5..9]

  main = print =<< findMaxFeedback <$> readFile "07.txt"
