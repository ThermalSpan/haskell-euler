module ProjectEuler.Problem004 (solution004) where

import Util

isIntPal :: Int -> Bool
isIntPal n = isPalindrome (show n)

maxf :: Int -> Int
maxf n = maximum [ x*y | x <- xs, y <- xs, isIntPal (x*y)]
                 where xs = [10^(n-1)..10^n -1]

solution004 :: Int
solution004 = maxf 3
