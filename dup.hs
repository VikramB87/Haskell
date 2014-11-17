
import Data.Set

dedup [] _ = []
dedup (x:xs) elems = if member x elems then dedup xs elems
                     else x:(dedup xs (insert  x elems))
main = do
        a <- getLine
        putStrLn $ dedup a (fromList "")
