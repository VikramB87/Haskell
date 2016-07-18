import Data.Map as Map

operator_map = Map.fromList [("+", (+)), ("-", (-)), ("*", (*)), ("/", (div))]

operator op = operator_map Map.! op

parseInt str = read str::Integer

partition = Prelude.foldr (\x(l,r) -> (x:r, l)) ([],[])

compute res opnd_list [] = res
compute res opnd_list oprt_list = compute ((head oprt_list) res (head opnd_list)) (tail opnd_list) (tail oprt_list)

calculate str = compute (head opnd_num) (tail opnd_num) (Prelude.map operator oprt)
                    where   (opnd, oprt) = Main.partition $ words $ takeWhile (\c -> c /= '=') str
                            opnd_num = Prelude.map parseInt opnd

main = do
        n <- getLine
        processTestCase $ parseInt n

processTestCase n = do
                        x <- getLine
                        s <- getLine
                        putStrLn $ show $ calculate s
                        if n > 1 then processTestCase (n-1) else return ()
