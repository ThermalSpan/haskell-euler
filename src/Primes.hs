{-
    Primes.hs
    by Russell Bentley

    An lazily evaluated list of primes, based on "The Genuine Sieve of Eratosthenes"
    by Melissa O'Neil
-}
module Primes (primes) where

import Util
import SkewHeap

---------------------------------------------------------------------------------------
-- Sieve of Eratosthenes 
-- The First section contains the code for a true sieve of erasothenes
-- Its based on a skew heap implemention I wrote for this purpose
---------------------------------------------------------------------------------------

-- | 'insertPrime' is used to add the list of multiples to the heap.
insertPrime :: (Real k) => k -> [k] -> Heap k [k] -> Heap k [k]
insertPrime p xs = insert (sq p, map (*p) xs)

-- | 'adjustHeap' is used to remove the mulitples lower than the current number being "sieved"
adjustHeap :: (Real k) => k -> Heap k [k] -> Heap k [k]
adjustHeap cutOff heap | n < cutOff = adjustHeap cutOff (insert (n', ns) $ deleteMin heap) 
                       | otherwise  = heap
                       where
                            (n, (n':ns)) = minKeyValue heap

-- | 'sieve'' is the core the sieve, it uses the heap to keep track of the next multiples
sieve'                   :: (Real k) => [k] -> Heap k [k] -> [k]
sieve' [] _              = []
sieve' (x:xs) heap 
    | nextComposite <  x = sieve' (x:xs) (adjustHeap x heap)   
    | nextComposite == x = sieve' xs heap
    | otherwise          = x : sieve' xs (insertPrime x xs heap)
    where  
         nextComposite   = minKey heap

-- | BAM, here's the 'sieve'
sieve :: (Real k) => [k] -> [k]
sieve [] = []
sieve (x:xs) = x : sieve' xs (insertPrime x xs emptyHeap)

---------------------------------------------------------------------------------------
-- Factor Wheel
-- A simple optimization the sieve that cuts down the number of potencial primes
---------------------------------------------------------------------------------------

-- | A hardcoded wheel... temporary
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357 

-- | Spin takes a wheel list and generates a list of potencial primes. 
spin :: (Real k) => [k] -> k -> [k]
spin (x:xs) n = n : spin xs (n + x)

---------------------------------------------------------------------------------------
-- Primes 
-- Here are the exposed methods, a defualt primes list which uses a wheel of 2357 wheel
-- and a function that generates a list of primes using an arbitrary wheel.
---------------------------------------------------------------------------------------

primes = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)

