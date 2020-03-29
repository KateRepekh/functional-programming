module SimpsonIntegration
where

import Control.Parallel

simpsonCompute :: (Double -> Double) -> (Double, Double) -> Double -> Double
simpsonCompute f (a, h) i = 4.0 * (f (a + (i * h))) + 2.0 * (f (a + ((i + 1) * h)))

simpsonParallel :: (Double -> Double) -> (Double, Double) -> Double -> Double -> [Double]
simpsonParallel f (a, h) i n 
  | i + 2.0 == n = [simpsonCompute f (a, h) i]
  | otherwise    = par s1 (seq s2 (s1 : s2))
      where s1 = simpsonCompute f (a, h) i
            s2 = simpsonParallel f (a, h) (i + 2.0) n

simpson :: (Double -> Double) -> (Double, Double) -> Double -> Double
simpson f (a, b) n = (h / 3.0) * ((f a) + (f b) + sum (simpsonParallel f (a, h) 1.0 n))
  where h = (b - a) / n

integrateRecursive :: Double -> Double -> (Double, Double) -> (Double -> Double) -> Double -> Double
integrateRecursive prevIntegral prevN (a, b) f eps
  | eps > abs (prevIntegral - curIntegral) = curIntegral
  | otherwise                              = integrateRecursive curIntegral curN (a, b) f eps
  where curN = prevN * 3.0
        curIntegral = simpson f (a, b) curN


integrate :: (Double -> Double) -> (Double, Double) -> Double -> Double
integrate f (a, b) eps = integrateRecursive ((f a) + (f b)) 1.0 (a, b) f eps
