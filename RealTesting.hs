module RealTesting where

import Control.Monad
import Data.List
import Test.QuickCheck

class Space a where
  average :: [a] -> Gen a

instance Space () where
  average _ = return ()

instance Space Int where
  average xs =
    return ((n + (k `div` 2)) `div` k)
   where
    n = sum xs
    k = length xs 

instance (Space a, Space b) => Space (a,b) where
  average xys =
    liftM2 (,) (average (map fst xys)) (average (map snd xys))
  
instance (Space a, Space b) => Space (Either a b) where
  average es =
    frequency
    [ (l, Left  `fmap` average xs)
    , (r, Right `fmap` average ys)
    ]
   where
    l = length xs
    r = length ys
    
    xs = [ x | Left x  <- es ]
    ys = [ y | Right y <- es ]

instance Space a => Space [a] where
  average xss = toList `fmap` average (map fromList xss)
   where
    toList (Left _)       = []
    toList (Right (x,xs)) = x:xs

    fromList []     = Left ()
    fromList (x:xs) = Right (x,xs)

fix0 :: (Arbitrary a, Eq a) => (a -> a) -> Gen a
fix0 f = arbitrary >>= go 100
 where
  go 0 _        = arbitrary >>= go 100
  go n x
    | x == y    = return x
    | otherwise = go (n-1) y
   where
    y = f x

fix :: (Arbitrary a, Space a, Eq a) => (a -> a) -> Gen a
fix f = arbitrary >>= go 100
 where
  go 0 _        = arbitrary >>= go 100
  go n x
    | x == y    = return x
    | otherwise = average [x,y] >>= go (n-1)
   where
    y = f x

