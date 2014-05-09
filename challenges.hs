import System.Random

-- 1. Last Element of List
myLast xs = if null xs then error "List must not be empty" else 
                if null  $ tail xs then head xs else myLast $ tail xs
myLast2 []   = error "List must not be empty"
myLast2 (x:[]) = x
myLast2 (_:xs) = myLast2 xs

myLast3 = head . reverse

myLast4 xs = elementAt (length xs) xs

myLast5 xs = foldr1 (const id)
myLast6 xs = foldr1 (curry snd)

myLast7 xs = foldl1 (flip const)

-- 2. Last but one element of List
myButLast xs = if length xs < 2 then error "List must of length > 1" else
                if length (tail xs) == 2 then head $ tail xs else myButLast $ tail xs

myButLast2 []        = error "List must have length > 1"
myButLast2 (x:[])    = error "List must have length > 1"
myButLast2  (x:_:[]) = x
myButLast2 (_:x:xs)  = myButLast2 (x:xs)

myButLast3 xs = elementAt ((length xs) - 1) xs

myButLast4 xs = head . tail . reverse

-- 3. Kth element of a list

elementAt _ [] = error "Invalid index"
elementAt 1 (x:xs) = x
elementAt k xs = elementAt (k-1) $ tail xs

elementAt2 k xs = myLast $ take k xs

elementAt3 k xs = fst $ last $ zip xs [1..k]

-- 4. Number of elements in a list

myLength [] = 0
myLength (x:xs) = 1+myLength xs

myLength2 xs = foldl (\a _ -> a+1) 0 xs

-- 5. Reverse a list

myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]

-- 6. Is list palindrome?

isPalindrome xs = xs == myReverse xs

-- 7. Flatten a nested structure
data NestedList a = Elem a | List [NestedList a]

flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ (flatten (List xs))

-- 8. Eliminate consecutive duplicates

compress [] = []
compress (x:[]) = [x]
compress (x:y:xs) = if x == y then compress(y:xs) else x:compress(y:xs)

-- 9. Pack consecutive duplicates into sublists

pack [] = []
pack all@(x:xs) = fst(pr):pack(snd(pr))
            where pr = span (\n -> n == x) all

-- 10. Run-length encoding

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack

-- 11. Modified run-length encoding

data RunLengthType a = Single a | Multiple Int a deriving (Show)
encode2 :: (Eq a) => [a] -> [RunLengthType a]
encode2 = map (\xs -> if (length xs) == 1 then Single (head xs) else Multiple (length xs) (head xs)) . pack

-- 12. Decode a run-length endoded array of problem 11

decode2 [] = []
decode2 ((Single x):xs) = x : decode2 xs
decode2 ((Multiple n x):xs) = replicate n x ++ decode2 xs

-- 13. Run-length encoding - direct

-- TODO

-- 14. Duplicate elements of a list
dupli [] = []
dupli (x:xs) = x:x:dupli(xs)

-- 15. Replicate elements of a list

repli _ 0 = []
repli xs 1 = xs
repli [] n = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- 16. Drop every Nth element from list

dropEvery _ 0 = error "n should be > 0"
dropEvery xs n = dropEveryHelper xs 1
                where   dropEveryHelper [] _ = []
                        dropEveryHelper xs c = if (mod c n == 0) then (dropEveryHelper (tail xs) (c+1)) else ((head xs):dropEveryHelper (tail xs) (c+1))

-- 17. Split list into two parts

split xs n = splitHelper [] xs 0
            where   splitHelper ys [] _ = (ys, [])
                    splitHelper ys xs c = if n == c then (ys, xs) else splitHelper (ys ++ [(head xs)]) (tail xs) (c+1)

-- 18. Extract a slice from a list

slice xs from to = sliceHelper [] xs 1
            where sliceHelper ys [] _ = ys
                  sliceHelper ys (x:xs) c
                    | c < from = sliceHelper ys xs (c+1)
                    | c > to   = ys
                    | otherwise = sliceHelper (ys ++ [x]) xs (c+1)

-- 19.
-- TODO
-- 20. Remove K'th element from list

removeAt xs k = removeAtHelper [] xs 1
                where removeAtHelper ys [] _ = ys
                      removeAtHelper ys (x:xs) n
                        | n == k = ys ++ xs
                        | otherwise = removeAtHelper (ys ++ [x]) xs (n+1)

-- 21. Insert element at a given position in list

insertAt elem xs n = insertAtHelper [] xs 1
                    where insertAtHelper ys [] _ = ys ++ [elem]
                          insertAtHelper ys (x:xs) c
                            | c < n = insertAtHelper (ys ++ [x]) xs (c+1)
                            | c == n = ys ++ [elem] ++ (x:xs)

-- 22. Create list of integers within given range

range from to = if from > to then [] else from:range (from+1) to

-- 23a. Extract a given number randomly from a list of numbers

random_select_elem [] = error "List length should be > 0"
random_select_elem xs = do
                    num <- getStdRandom (randomR(0, (length xs)-1))
                    return $ xs!!num

-- 23b. Extract a given number of randomly selected elements from a list

--random_select :: (Show a) => [a] -> Int -> IO [a]
random_select xs n = if (n == 0 || null xs) then return []
                    else
                        do
                            num <- getStdRandom (randomR(0, (length xs)-1))
                            ys <- (random_select (removeAt xs (num+1)) (n-1))
                            return $ (xs!!num) : ys

-- 24. Draw N different random numbers from the set 1..M

random_select2 n m = random_select [1..m] n


-- 25. Generate Random Permutation of Elements

permute xs = if null xs then return []
             else
                do
                    num <- getStdRandom (randomR(0, (length xs)-1))
                    ys <- permute $ removeAt xs (num+1)
                    return $ (xs!!num) : ys

-- 26. Generate all combinations of a given size of a list

combinations _ [] = []
combinations 0 _  = []
combinations n xs = snd $ foldl (\(i, ys) x -> (i+1, ys++(removeAt xs i))) (1, []) xs

