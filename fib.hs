import Control.Monad

fib n = if n < 3 then n-1 else fib_helper 3 1 0
            where
            fib_helper m last plast = if m == n then last+plast
                                      else fib_helper (m+1) (last+plast) last

prefix xs ys = prefix_helper "" xs ys
                where
                    prefix_helper comm "" "" = (comm, "", "")
                    prefix_helper comm xs "" = (comm, xs, "")
                    prefix_helper comm "" ys = (comm, "", ys)
                    prefix_helper comm a@(x:xs) b@(y:ys) = if x == y then prefix_helper (comm++[x]) xs ys else (comm, a, b)

main = do
        s1 <- getLine
        s2 <- getLine
        let (comm, first, second) = prefix s1 s2 in
            mapM putStrLn (map (\str -> ((show (length str)) ++ str)) [comm, first, second])
