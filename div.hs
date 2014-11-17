
import Data.Set as Set

isPrime :: Int -> Bool
isPrime n = not $ any (\x -> n `mod` x == 0) [2.. floor  $ sqrt $ fromIntegral n]

nextPrime n = if isPrime (n+2) then n+2 else nextPrime (n+2)

divisors n = Prelude.filter (\x -> n `mod` x == 0) [1..n]

commonDivisors m n = toList $ Set.intersection (Set.fromList $ divisors m) (Set.fromList $ divisors n)

main = do
        input <- getLine
        input <- getContents
        print $ Prelude.map (\line -> Prelude.map (\w -> read w::Int) (words line)) (lines input)
