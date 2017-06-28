module Optimize where

import Data.List

type Point = [Double]

{-
Typical use of minimize:

  goal p . giveUp k . take n . minimize (repeat d) xs $ h

Here:

- p :: a -> Bool
  what result value are we looking for
  
- k :: Int
  how many times can the result get worse before we cut our losses

- n :: Int
  maximum number of iterations

- d :: Double
  size of "the box"

- xs :: [Double]
  starting point

- h :: [Double] -> a
  function to minimize
-}

-- helpers
goal :: (a -> Bool) -> [(Point,a,a)] -> (Point,a)
goal p ((vs,x,_):qs)
  | null qs || p x = (vs,x)
  | otherwise      = goal p qs
goal p []          = error "goal []"

giveUp :: Ord a => Int -> [(Point,a,a)] -> [(Point,a,a)]
giveUp n = go n
 where
  go 0 _  = []
  go k (q@(vs,x,y):qs@((vs',x',y'):_))
    | x == x' && y' >= y = q : go (k-1) qs
    | otherwise          = q : go n     qs
  go _ qs = qs

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
      then go (sort ((x0,p0):[ (h p',p') | (_,p) <- tail xps, let p' = p -->+ p0 ]))
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

