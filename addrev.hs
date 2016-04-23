-- http://www.spoj.com/problems/ADDREV/
main = do
        input <- getLine
        input <- getContents
        mapM (putStrLn.show.reverseInt.revfun.words) (lines input)

reverseInt = parse.reverse.show
revfun a = sum (map (parse.reverse) a)
parse n = read n::Int
