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
  how many times in a row can the result get worse before we give up

- n :: Int
  maximum number of iterations

- d :: Double
  size of "the box", the initial jumps that will be taken

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

-- produces a possibly infinite list of (point,best-result,worst-result)
minimize :: Ord a => Point -> Point -> (Point -> a) -> [(Point,a,a)]
minimize _  [] h = [([],x,x)] where x = h []
minimize box0 p h = go (sort [ pair p | p <- ps0 ])
 where
  -- trim box
  box = take (length p) box0

  -- initial points
  ps0 =
    map (zipWith (+) p) $
    map (zipWith (*) box) $
    initial (length box)

  initial n =
    replicate n 0:
    [ replicate i b ++ [a] ++ replicate (n-i-1) b
    | i <- [0..n-1] ]
    where
      nf :: Double
      nf = fromIntegral n

      b = (sqrt(nf+1)-1) / (nf * sqrt 2)
      a = b + 1 / sqrt 2

  -- pairing up result and point
  pair p = (h p, p)

  -- refactored from https://en.wikipedia.org/wiki/Nelder-Mead_method
  go xps =
    (p0,x0,xL) :
    if xR < xN then
      if x0 <= xR || xR <= xE then
        -- reflect
        go (insert qR xpsI)
      else
        -- expand
        go (insert qE xpsI)
    else
      if xC < xL then
        -- contract
        go (insert qC xpsI)
      else
        -- shrink
        go (sort (q0:[ pair (p -*-> (0.15,p0)) | (_,p) <- tail xps ]))
   where
    xpsI       = init xps
    q0@(x0,p0) = head xps
    qN@(xN,_)  = last xpsI
    qL@(xL,pL) = last xps

    -- centroid
    pO = centroid (map snd xpsI)

    -- reflect, expand, contract
    qR@(xR,_) = pair (pL -*-> (2,   pO))
    qE@(xE,_) = pair (pL -*-> (3,   pO))
    qC@(xC,_) = pair (pL -*-> (0.4, pO)) -- not 0.5 to avoid the same point twice

centroid :: [Point] -> Point
centroid ps = [ sum [p!!i | p <- ps] / fromIntegral l | i <- [0..l-1] ]
 where
  l = length ps

-- generic "towards": reflect (a=2), expand (a=3), contract (a=0.5), shrink (a=0.1)
(-*->) :: Point -> (Double, Point) -> Point
p -*-> (a,q) = [ x + a*(y - x) | (x,y) <- p `zip` q ]

