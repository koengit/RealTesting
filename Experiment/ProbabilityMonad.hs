module ProbabilityMonad where

import Optimize

import Numeric.Probability.Distribution hiding (map)

type Generator a = Int -> [Double] -> T Double a

freq :: Ord a => [(Double, a)] -> T Double a
freq fs = let tot = sum (map fst fs) in
  cons $ [ (a, f / tot ) | (f, a) <- fs ]

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Ord, Eq, Show)

poly :: [Double] -> Int -> Double
poly ps xi = let x = fromInteger (toInteger xi) in
  sum [ p * (x ** i) | (i, p) <- zip [0.0 ..] ps ]

depthBounded :: Generator (Tree ())
depthBounded d params = norm $ case d of
  0 -> return Leaf
  _ -> do
    left  <- depthBounded (d - 1) params
    right <- depthBounded (d - 1) params
    freq [(1, Leaf), (poly params d, Node () left right)]

sizeBounded :: Generator (Int, Tree ())
sizeBounded s params = norm $ case s of
  0 -> return (1, Leaf)
  _ -> do
    (sl, left)  <- sizeBounded (s - 1) params
    (sr, right) <- sizeBounded (s - sl) params 
    freq [(1, (1, Leaf)), (poly params s, (sl + sr, Node () left right))]

quality :: Ord a => T Double a -> Double
quality t = minimum (map snd (decons (norm t)))

optimize n =
  fst $ minimize 1000 (\a b -> abs (a - b) < 1e-16)
                 [1,1,1]
                 ((5*) <$> [1,1,1])
                 (negate . quality . (depthBounded n))
optimize' n =
  fst $ minimize 1000000 (\a b -> abs (a - b) < 1e-16)
                 [1,1,1,1,1,1,1]
                 ((2*) <$> [1,1,1,1,1,1,1])
                 (negate . quality . (fst <$>) . (sizeBounded n))

howGood :: Ord a => T Double a -> Double
howGood t = q / opt
  where
    q   = quality (norm t)
    opt = (1 / (fromInteger (toInteger (size (norm t)))))

{- To see how good an optimizer we can make with this basic strategy:
 -  howGood (depthBounded n (optimize n))
 -  howGood (sizeBounded  n (optimize' n))
 -}
