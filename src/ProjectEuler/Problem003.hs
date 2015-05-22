module ProjectEuler.Problem003 (solution003) where

import Util

findFactor :: Integer -> [Integer] -> Integer
findFactor _ [] = 1
findFactor n (p:xs) = if n `mod` p ==0 then p else findFactor n xs

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = p : primeFactors (n `div` p) 
                 where p = findFactor n primes

solution003 :: Integer
solution003 = maximum (primeFactors 600851475143)
