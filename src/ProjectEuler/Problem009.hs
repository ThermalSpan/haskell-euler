module ProjectEuler.Problem009 (solution009) where

import Util

trips ::  Int -> [Int]
trips n = [ x*y*z | x<-[1..n], y<-[1..n], 
            let s = sq x + sq y,
            let t = (sqrt.fromIntegral) s :: Double ,
            let z = floor t :: Int,
            z*z==s, x+y+z==n]

solution009 :: Int
solution009 = head (trips 1000)
