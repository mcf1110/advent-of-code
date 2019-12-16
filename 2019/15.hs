module Day15 where
    import qualified IntCode as IC
    import qualified Data.Map as M
    import qualified Data.Matrix as Mx

    import Debug.Trace

    type Coord = (Int, Int)
    type DroneMap = M.Map Coord Int
    data Move = U | D | L | R deriving (Show, Enum)

    undo :: Move -> Move
    undo U = D
    undo D = U
    undo L = R
    undo R = L

    apply :: Coord -> Move -> Coord
    apply (x,y) U = (x, y+1)
    apply (x,y) D = (x, y-1)
    apply (x,y) L = (x-1, y)
    apply (x,y) R = (x+1, y)
    (#) = apply

    command :: IC.ProgramState -> Move -> (IC.ProgramState, Int)
    command pgSt move = case IC.runUntilOutput $ IC.addInputs [1 + fromEnum move] pgSt of
        Nothing -> error "HALT"
        Just x -> x


    search :: Int -> [Int] -> IC.ProgramState -> DroneMap -> Coord -> (DroneMap, IC.ProgramState, [Int])
    search distToOrigin distances pgSt m origin = foldr lookup (m, pgSt, distances) order
        where 
            order = reverse [U, R, D, L]
            lookup :: Move -> (DroneMap, IC.ProgramState, [Int]) -> (DroneMap, IC.ProgramState, [Int])
            lookup direction (mp, pgSt, ds) = case mp M.!? origin' of
                Nothing -> case i of
                    0 -> (map', pgSt', ds)
                    _ -> (newMap, backPgSt, newDs)
                Just _ -> (mp, pgSt, ds)
                where
                    origin' = origin # direction
                    map' = M.insert origin' i mp
                    (pgSt', i) = command pgSt direction
                    ds' = if i == 2 then distToOrigin:ds else ds
                    (newMap, newPgSt, newDs) = search (distToOrigin + 1) ds' pgSt' map' origin'
                    (backPgSt, _) = command newPgSt (undo direction)


    initialMap :: DroneMap
    initialMap = M.singleton (0,0) 1

    mapToDistance :: DroneMap -> Coord -> DroneMap -> DroneMap
    mapToDistance maze origin distMap = foldr lookup distMap order
        where 
            order = reverse [U, R, D, L]
            lookup :: Move -> DroneMap -> DroneMap
            lookup direction mp = case mp M.!? origin' of
                Just _ -> mp
                Nothing -> if maze M.! origin' == 0 then mp else newMap
                where
                    distToOrigin = mp M.! origin
                    origin' = origin # direction
                    map' = M.insert origin' (distToOrigin + 1) mp
                    newMap = mapToDistance maze origin' map'

    main :: IO ()
    main = do
        str <- readFile "15.txt"
        let pg = IC.strToProgram str []
        let (completeMap, final, ds) = search 1 [] pg initialMap (0,0)
        print $ minimum ds
        let oxygenCoord = head $  M.keys $ M.filter (==2) completeMap
        print $ maximum $ M.elems $ mapToDistance completeMap oxygenCoord (M.singleton oxygenCoord 0)