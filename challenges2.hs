import Data.List
import Data.Maybe

-- 31
--
-- 32. Determine the GCD of two numbers

myGCD a b = if b == 0 then a else myGCD b (a `mod` b)

-- 33. Are two numbers co-prime?

coPrime a b = (myGCD a b) == 1


-- 34. Euler's Totient Function

totient m = length $ filter (\x -> coPrime x m) [1..m]

-- 35. Is a number prime?

isDivisible n = \x -> n `mod` x == 0

isPrime :: Integer -> Bool
isPrime n = not $ any (isDivisible n) [2..m]
            where m = floor $ sqrt $ fromIntegral n

nextPrime n = if (x == 3 || isPrime x) then x else nextPrime x
                where x = n+1

-- 35a. Prime factors of a number

--primeFactors :: Integer -> [Integer]
primeFactors n = sort $ primeFactorsHelper n 2
                    where primeFactorsHelper x a = if isPrime(x) then [x]
                                                    else if isDivisible x a then a:(primeFactorsHelper  (quot x a) a)
                                                    else primeFactorsHelper x (nextPrime a)

-- 36. Prime factors with their multiplicity

primeFactors_mult = map (\(a,b) -> (b,a)) . encode . primeFactors
                    where encode :: (Eq a) => [a] -> [(Int, a)]
                          encode = map (\xs -> (length xs, head xs)) . pack

pack [] = []
pack all@(x:xs) = fst(pr):pack(snd(pr))
            where pr = span (\n -> n == x) all

-- 37. Improved totient function

-- ???
totient2 n = foldl (\s (p, m) -> s * p*((p-1)^(m-1))) 1 (primeFactors_mult n)

-- 39. List of prime numbers in a limit

primesR a b = filter isPrime [a..b]

-- 40. Goldbach's conjecture

goldBach n = (x, n-x)
                where x = case find (\x -> isPrime (x) && isPrime (n-x)) [2..n] of
                            Just n -> n
                            Nothing -> error "Goldbach was wrong!"
