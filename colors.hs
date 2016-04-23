
pfx nr ng ny nb [] = (nr == ng) && (ny == nb)
pfx nr ng ny nb (x:xs) = case x of
                                'R' -> if nr - ng > 0 then False else pfx (nr+1) ng ny nb xs
                                'G' -> if ng - nr > 0 then False else pfx nr (ng+1) ny nb xs
                                'Y' -> if ny - nb > 0 then False else pfx nr ng (ny+1) nb xs
                                'B' -> if nb - ny > 0 then False else pfx nr ng ny (nb+1) xs

prefix = pfx 0 0 0 0

main = do
        input <- getLine
        input <- getContents
        mapM (putStrLn . show) $ map prefix (lines input)


