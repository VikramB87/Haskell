-- http://www.spoj.com/problems/HS12MBR/
data Shape = Point Int Int | Circle Int Int Int | LineSegment Int Int Int Int
data Rectangle = Rectangle Int Int Int Int
instance Show Rectangle where
    show (Rectangle x1 y1 x2 y2) = (show x1) ++ " " ++ (show y1) ++ " " ++  (show x2) ++ " " ++ (show y2)

main = do
        t <- getLine
        processTestCase $ parse t

processTestCase t = do
                        n <- getLine
                        input <- readObjects (parse n) []
                        let r = (map (mbr.createShape.split ' ') input) in
                            putStrLn $ show $ foldl minRect (head r) (tail r)
                        if t > 1 then do
                            x <- getLine
                            processTestCase (t-1)
                        else return ()

readObjects n xs = do
                    o <- getLine
                    let xs' = o : xs in
                        if (n > 1) then (readObjects (n-1) xs') else return xs'

parse n = read n::Int

split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

createShape ("p":x:y:[])         = Point (parse x) (parse y)
createShape ("c":x:y:r:[])       = Circle (parse x) (parse y) (parse r)
createShape ("l":x1:y1:x2:y2:[]) = LineSegment (parse x1) (parse y1) (parse x2) (parse y2)

mbr (Point x y)               = Rectangle x y x y
mbr (Circle x y r)            = Rectangle (x-r) (y-r) (x+r) (y+r)
mbr (LineSegment x1 y1 x2 y2) = Rectangle (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)

minRect (Rectangle x11 y11 x12 y12) (Rectangle x21 y21 x22 y22) = Rectangle (min x11 x21) (min y11 y21) (max x12 x22) (max y12 y22)
