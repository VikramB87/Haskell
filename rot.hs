
rotate lst = if null lst then [] else tail lst ++ [head lst]

rotateN lst n = if n == 0 then []
                else
                    let rot = rotate lst
                    in rot : (rotateN rot (n-1))

allRotations s = rotateN s (length s)

concatStr = concatMap (\x -> x ++ " ")

main = do
        input <- getLine
        input <- getContents
        mapM putStrLn $ map (\str -> concatStr $ allRotations str) (lines input)
