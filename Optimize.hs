module Optimize where

import Data.List

type Point = [Double]

minimize :: Ord a
         => (a -> a -> Bool) -> Point -> Point -> (Point -> a) -> (Point,a)
minimize stop box p h = go (sort [ (h p, p) | p <- ps ])
 where
  ps = p : [ take i p ++ [x] ++ drop (i+1) p
           | (x,i) <- zipWith (+) p box `zip` [0..]
           ]

  go xps
    | stop x0 x = (p0,x0)
    | x' > x    = go (sort [ (h p',p') | (_,p) <- xps, let p' = p -->+ p0 ])
    | otherwise = go (insert (x',p') xps')
   where
    (x0,p0) = head xps
    xps'    = init xps
    (x,p)   = last xps
    p'      = p -+-> centroid (map snd xps')
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

