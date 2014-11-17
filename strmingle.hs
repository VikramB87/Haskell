

main1 = do
        a <- getLine
        b <- getLine
        putStrLn (mingle a b)
       where
        mingle [] [] = []
        mingle (x:xs) (y:ys) = x:y:mingle xs ys

main = do
        a <- getLine
        putStrLn $ compress a

pack [] = []
pack all@(x:xs) = fst(pr):pack(snd(pr))
                    where pr = span (\n -> n == x) all
compress xs = foldl (\r c -> r ++ c) ""  (map (\xs -> if (length xs) == 1 then [(head xs)] else (head xs):(show (length xs))) (pack xs))

gcd' m n = if n == 0 then m else gcd' n (mod m n)
