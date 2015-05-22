module ProjectEuler.Problem002 (solution002) where

pairSum :: Num a => (a, a) -> a
pairSum (x, y) = x + y

fib :: [Integer]
fib = 0 : 1 : (map pairSum (zip fib (tail fib)))

solution002 :: Integer
solution002 = sum (filter even (takeWhile (<= 4000000) fib))
