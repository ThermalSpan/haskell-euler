module ProjectEuler.Problem006 (solution006) where

import Util

sumOfSqr :: Int -> Int
sumOfSqr n = sum (take n [ sq x | x <- [1..]])

sqrOfSum :: Int -> Int
sqrOfSum n = sq (sum (take n [1..]))

dif :: Int -> Int
dif n = (sqrOfSum n) - (sumOfSqr n)

solution006 :: Int
solution006 = dif 100

