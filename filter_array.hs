import Debug.Trace

f :: Int -> [Int] -> [Int]
f n arr = if null arr then [] else if head(arr) < n then head(arr):(f n (tail arr))
                     else (f n (tail arr))

myrepeat n arr = if null arr then []
                else (repeat_elem n (head arr)) ++ (myrepeat n (tail arr))
               where
                 repeat_elem n elem = take n (repeat elem)

filter_odd_pos lst = filter_odd_pos_helper True lst
                    where
                        filter_odd_pos_helper is_odd lst = if null lst then []
                                                           else if is_odd then (filter_odd_pos_helper False (tail lst))
                                                           else (head lst):(filter_odd_pos_helper True (tail lst))

rev lst = if null lst then [] else rev(tail lst) ++ [head lst]

sum_odd xs = if null xs then 0
             else if is_odd (head xs) then (head xs) + sum_odd (tail xs)
             else sum_odd (tail xs)
            where
                is_odd x = x `mod` 2 == 1




exp2 x = let
            exp_helper sum numr denr term = if term > 10 then sum
                                             else exp_helper (sum + (numr/denr)) (numr*x) (denr*term) (term+1)
        in
             exp_helper 1.0 x 1.0 2.0
