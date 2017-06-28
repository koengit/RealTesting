module Optimize where

import Data.List
import Data.Ord

type Point = [Double]

minimize :: Ord a
         => Int -> (a -> a -> Bool) -> Point -> Point -> (Point -> a) -> (Point,a)
minimize c stop box p h = go c (sort [ (h p, p) | p <- ps ])
 where
  ps = p : [ take i p ++ [x] ++ drop (i+1) p
           | (x,i) <- zipWith (+) p box `zip` [0..]
           ]

  go n xps
    | n == 0 || stop x0 x = (p0,x0)
    | x' > x    = go (n - 1) (sort [ (h p',p') | (_,p) <- xps, let p' = p -->+ p0 ])
    | otherwise = go (n - 1) (insert (x',p') xps')
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

patternSearch :: Int                  -- ^ Number of steps
              -> [Double]             -- ^ Initial point
              -> Double               -- ^ Initial delta
              -> Double               -- ^ Epsilon
              -> ([Double] -> Double) -- ^ Function to minimize
              -> ([Double], [[Double]])
patternSearch = patternMinimizeInner []

setAt :: [Double] -> Int -> Double -> [Double]
setAt xs i d = take i xs ++ [d] ++ (drop (i + 1) xs)

patternMinimizeInner :: [[Double]]           -- ^ Acc
                     -> Int                  -- ^ Number of steps
                     -> [Double]             -- ^ Initial point
                     -> Double               -- ^ Initial delta
                     -> Double               -- ^ Epsilon
                     -> ([Double] -> Double) -- ^ Function to minimize
                     -> ([Double], [[Double]])
patternMinimizeInner acc 0 p _ _ _  = (p, reverse $ p:acc)
patternMinimizeInner acc n p d eps f
  | d < eps   = (p, reverse $ p:acc)
  | otherwise =
    let lows   = [ setAt p i (p !! i + d) | i <- [0 .. length p - 1] ]
        highs  = [ setAt p i (p !! i - d) | i <- [0 .. length p - 1] ]
        points = lows ++ highs
        minP   = minimumBy (comparing f) points
    in if f p <= f minP then
         patternMinimizeInner (p:acc) (n - 1) p (d / 2) eps f
       else
         patternMinimizeInner (p:acc) (n - 1) minP d eps f
