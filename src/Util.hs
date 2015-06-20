module Util where 

import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

--Infinite Data Structure for Primes
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs


clump :: Int -> [a] -> [[a]]
clump _ [] = []
clump n (x:xs) =  [x : take (n-1) xs] ++ if length xs >= n then clump n xs else []

pairSum :: Num a => (a, a) -> a
pairSum (x, y) = x + y

first :: (a,b) -> a
first (x,_) = x

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x ==0]

sq :: Num a => a -> a
sq x = x*x

-- 
time :: a -> IO a
time x = do
    start <- getTime Monotonic
    y     <- evaluate x
    end   <- getTime Monotonic
    fprint timeSpecs  start end
    putStrLn ""
    return y


