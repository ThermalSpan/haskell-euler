module ProjectEuler.Problem005 (solution005) where

divby :: Int -> Int -> Bool
divby x y = y `mod` x == 0

sievef :: Int -> [Int] -> [Int]
sievef 1 xs = xs
sievef n xs = sievef (n-1) (filter (divby n) xs)

gcdn :: Int -> Int
gcdn n = head (sievef (n-1) [ k*n | k <- [1..]]) 

solution005 :: Int
solution005 = gcdn 20
