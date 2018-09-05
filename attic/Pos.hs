{-# LANGUAGE ImplicitParams #-}
module Pos where

import Optimize

class Pos a where
  times :: Double -> a -> a
  plus :: a -> a -> a

instance Pos Double where
  times = (*)
  plus = (+)

instance (Pos a, Pos b) => Pos (a, b) where
  times x (y, z) = (times x y, times x z)
  (x, y) `plus` (x', y') = (x `plus` x', y `plus` y')

pos :: (?eps :: Double) => Double -> Double
pos x
  | ?eps < 0.00001 = if x >= 0 then 1 else 0
  | otherwise = (1 + tanh (x / ?eps)) / 2

ifPos :: (?eps :: Double, Pos a) => Double -> a -> a -> a
ifPos x y z =
  times (pos x) y `plus` times (1-pos x) z

ifGt :: (?eps :: Double, Pos a) => Double -> Double -> a -> a -> a
ifGt x y z w =
  ifPos (x - y) z w

ifLt :: (?eps :: Double, Pos a) => Double -> Double -> a -> a -> a
ifLt x y z w =
  ifPos (y - x) z w

f :: (?eps :: Double) => Double -> Double
f x =
  ifGt x (-30) 100
    (ifLt x (-31) 10 (-10))

fe :: Double -> Double -> Double -> Double
fe e1 e2 x =
  let ?eps = e1 in ifGt x (-30) 100 (let ?eps = e2 in ifLt x (-31) 10 (-10))

griewank :: [Double] -> Double
griewank xs =
  1 + sum (map (^2) xs) / 4000 -
  product [ cos (x / sqrt i) | (x, i) <- zip xs [1..] ]

minimiseGriewank d =
  last $ take n $ minimize ds xs griewank
  where
    n = 1000
    ds = replicate d 100
    xs = replicate d 10

main =
  print $
    last . take n . minimize ds xs $ h
  where
    k = 1000
    n = 10000
    ds = [100,100,50]
    xs = [1000,1000,0]
    h [e1,e2,x] =
      mul e1' * mul e2' * fe e1' e2' x
     where
      e1' = abs e1
      e2' = abs e2
    mul e = 1 + log (1+100*e)


