module Optimize where

import Data.List

type Point = [Double]

-- helper function, most common use (?)
stopping :: (a -> Bool) -> [a] -> a
stopping p [x]                = x
stopping p (x:xs) | p x       = x
                  | otherwise = stopping p xs

-- produces an infinite list of (point,best-result,worst-result)
minimize :: Ord a => Point -> Point -> (Point -> a) -> [(Point,a,a)]
minimize box p h = go (sort [ (h p, p) | p <- ps ])
 where
  ps = p : [ take i p ++ [x] ++ drop (i+1) p
           | (x,i) <- zipWith (+) p box `zip` [0..]
           ]

  go xps =
    (p0,x0,x1) :
    if x' > x1
      then go (sort [ (h p',p') | (_,p) <- xps, let p' = p -->+ p0 ])
      else go (insert (x',p') xps')
   where
    (x0,p0) = head xps
    xps'    = init xps
    (x1,p1) = last xps
    p'      = p1 -+-> centroid (map snd xps')
    x'      = h p'

centroid :: [Point] -> Point
centroid ps = [ sum [p!!i | p <- ps] / fromIntegral l | i <- [0..l-1] ]
 where
  l = length ps

-- mirror
(-+->) :: Point -> Point -> Point
p -+-> q = [ 2*y - x | (x,y) <- p `zip` q ]

-- move towards
(-->+) :: Point -> Point -> Point
p -->+ q = [ x + 0.2*(y-x) | (x,y) <- p `zip` q ]

