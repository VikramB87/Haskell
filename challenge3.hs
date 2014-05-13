
-- 46. Printing truth table for an expression in two variables

table f = let res = map (\pr -> ((fst pr), (snd pr), (uncurry f) pr)) [(x, y) | x <- [True, False], y <- [True, False]]
              dispStr b = if b then "T" else "F"
          in sequence $ map (\(a,b,c) -> putStrLn (dispStr a ++ " " ++ dispStr b ++ " " ++ dispStr c)) res

-- 47. Truth table for n variables

join [] = []
join (xs:[]) = map (\x -> [x]) xs
join (xs:xss) = xs >>= (\x -> joinElem x (join xss))
                where joinElem x xs = map (\ys -> x:ys) xs

tableN n f = let values = join (map (\_ -> [True, False]) [1..n])
                 dispStr b = if b then "T" else "F"
                 valStr = foldl (\a x -> a ++ dispStr x ++ " ") ""
             in
                sequence $ map (\xs -> putStrLn xs) (map (\xs -> (valStr xs) ++ dispStr (f xs)) values)

