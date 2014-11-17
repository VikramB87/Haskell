
main = do
        input <- getLine
        input <- getLine
        print (foldl1 lcm (map read $ words input :: [Int]))
