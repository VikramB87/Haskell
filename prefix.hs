
common p [] ys = [ p, [], ys ]
common p xs []  = [ p, xs, [] ]
common p (x:xs) (y:ys) = if x == y then (common (p++[x]) xs ys)
                         else [ p, x:xs, y:ys ]

main = do
        l1 <- getLine
        l2 <- getLine
        mapM (\x -> putStrLn $ (show (length x) ++ " " ++ x)) (common "" l1 l2)
