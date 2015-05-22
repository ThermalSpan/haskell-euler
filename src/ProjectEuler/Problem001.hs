module ProjectEuler.Problem001 (solution001) where

sumf :: Int -> Int
sumf n = sum [ x | x <- [1..(n-1)], (x `mod` 3 == 0) || (x `mod` 5 == 0)]

solution001 :: Int
solution001 = sumf 1000

