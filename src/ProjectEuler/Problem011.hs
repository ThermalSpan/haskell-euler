module ProjectEuler.Problem011 (solution011) where

import Util

takeWhilePN :: Int -> (a -> Bool) -> [a] -> [a]
takeWhilePN _ _ [] = []
takeWhilePN n p (x:xs) | p x       = x : takeWhilePN n p xs
                       | otherwise = take n (x:xs)

triangles :: [Int]
triangles = 1 : map pairSum (zip triangles [2..])

firstWithN n = first (zip triangles (takeWhilePN 1 (\xs -> length xs <= n) (map factors triangles))))
