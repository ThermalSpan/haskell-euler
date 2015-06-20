module ProjectEuler.Problem010 (solution010) where

import Util
import Primes

solution010 :: Integer
solution010 = sum (takeWhile (< 2000000) primes)
