module Day09 where 
    import IntCode
    main = do
        pg <- readFile "09.txt"
        print $ run pg [1]
        print $ run pg [2]