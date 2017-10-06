{-# LANGUAGE ImplicitParams #-}
module Pos where

import Data.List( sort )
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
  | x == 0    = 0
  | otherwise = (1+(x / (?eps + abs x)))/2

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

main =
  putStr $ unlines $ map show $
    take n . minimize ds xs $ h
  where
    k = 1000
    n = 500
    ds = [1000,1000,-400]
    xs = [0,0,200] --[1000,1000,0]
    h [e2,e1,x] =
      (fe e1' e2' x, reverse (sort [e1', e2']))
     where
      e1' = 0 `max` e1
      e2' = 0 `max` e2
    mul e = (1+e)**0.5


