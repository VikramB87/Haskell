
import qualified Data.Map as Map

makemap [] m = m
makemap (x:xs) m = case Map.lookup x m of
                    Nothing -> makemap xs (Map.insert x 1 m)
                    Just n -> makemap xs (Map.insert x (n+1) m)


mapdiff m1 m2 = foldl (diff) [] (Map.toList m1)
                where
                    diff lst (key, count) = case Map.lookup key m2 of
                        Nothing -> lst++[key]
                        Just n -> if (n < count) then lst++(take (count-n) (repeat key)) else lst

main = do
        l1 <- getLine
        l1 <- getLine
        l2 <- getLine
        l2 <- getLine
        let m1 = makemap (map read $ words l1 :: [Int]) Map.empty
        let m2 = makemap (map read $ words l2 :: [Int]) Map.empty
        mapM (\x -> putStr $ (show x) ++ " ") $ mapdiff m2 m1
