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
  ifGt x (-30) 10
    (ifLt x (-31) 10 (-10))

main =
  print $
    last . giveUp k . take n . minimize ds xs $ h
  where
    k = 100
    n = 1000
    ds = [1000, 1]
    xs = [10000, 0]
    h [eps, x] =
      let ?eps = abs eps in (f x, ?eps)
