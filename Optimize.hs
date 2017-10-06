{-# LANGUAGE BangPatterns #-}
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
minimize :: Ord a => Point -> Point -> (Point -> a) -> [(Step,Point,a,a)]
minimize box p h = minimize' box p (\_ -> h)

data Step = Init | Reflect | Expand | Contract | Shrink deriving ( Eq, Ord, Show )

minimize' :: Ord a => Point -> Point -> (Int -> Point -> a) -> [(Step,Point,a,a)]
minimize' _  [] h = [(Init,[],x,x)] where x = h 0 []
minimize' box p h = go Init 0 (sort [ pair 0 p | p <- ps0 ])
 where
  -- initial points
  ps0 = p : [ take i p ++ [x] ++ drop (i+1) p
            | (x,i) <- zipWith (+) p box `zip` [0..]
            ] ++
            [ take i p ++ [x] ++ drop (i+1) p
            | (x,i) <- zipWith (+) p (map (*0.35) box) `zip` [0..]
            ]

  -- pairing up result and point
  pair i p = (h i p, p)

  -- refactored from https://en.wikipedia.org/wiki/Nelder-Mead_method
  go step !i xps =
    (step,p0,x0,xL) :
    if xR < xN then
      if x0 <= xR || xR <= xE then
        -- reflect
        go Reflect i (insert qR xpsI)
      else
        -- expand
        go Expand (i+1) (insert qE xpsI)
    else
      if xC < xL then
        -- contract
        go Contract (i+1) (insert qC xpsI)
      else
        -- shrink
        go Shrink (i+1) (sort (q0:[ pair i (p -*-> (0.15,p0)) | (_,p) <- tail xps ]))
   where
    --xps        = [ pair i p | (_,p) <- xps' ]
    xpsI       = init xps
    q0@(x0,p0) = head xps  -- best point
    qN@(xN,_)  = last xpsI -- second-to-worst point
    qL@(xL,pL) = last xps  -- worst point

    -- centroid
    pO = centroid (map snd xpsI)

    -- reflect, expand, contract
    qR@(xR,_) = pair i (pL -*-> (2,   pO))
    qE@(xE,_) = pair i (pL -*-> (3,   pO))
    qC@(xC,_) = pair i (pL -*-> (0.4, pO)) -- not 0.5 to avoid the same point twice

centroid :: [Point] -> Point
centroid ps = [ sum [p!!i | p <- ps] / fromIntegral l | i <- [0..l-1] ]
 where
  l = length ps

-- generic "towards": reflect (a=2), expand (a=3), contract (a=0.5), shrink (a=0.1)
(-*->) :: Point -> (Double, Point) -> Point
p -*-> (a,q) = [ x + a*(y - x) | (x,y) <- p `zip` q ]

