module Main where

import Test.QuickCheck
import VBool
import Data

ship :: [Double] -> [Double]
ship accs = xs
 where
  vels = integrate accs
  xs   = integrate vels

integrate :: [Double] -> [Double]
integrate xs = tail ys where ys = 0 : zipWith (+) xs ys

checks :: [Double] -> VBool
checks [] = false
checks xs =
  head $
  eventually ( map (>=%100) xs'
           &&* eventually ( map (<=%(-100)) xs'
                        &&* eventually ( map (>=%100) xs' )
                          )
             )
 where
  xs' = take 100 xs

(&&*) = zipWith (&&+)
(||*) = zipWith (||+)
nott  = map nt

wuntil :: [VBool] -> [VBool] -> [VBool]
(a:as) `wuntil` (b:bs) = (b ||+ (a &&+ x)) : us
 where
  us = as `wuntil` bs
  x  = if null us then true else head us
_ `wuntil` _ = []

eventually :: [VBool] -> [VBool]
eventually xs = scanr1 (||+) xs

always :: [VBool] -> [VBool]
always xs = scanr1 (&&+) xs

prop_Ship :: [Block] -> VBool
prop_Ship blks =
  foldr (&&+) true [ (-1) <=% a &&+ a <=% 1 | a <- accs ] ==>%
    nt (checks (ship accs))
 where
  accs = concat [ replicate n x | Block n x <- blks ]

main = quickCheck (\bs -> forData bs prop_Ship)
--main = quickCheck (isTrue . prop_Ship)

data Block = Block Int Double
 deriving ( Eq, Ord, Show )

instance Arbitrary Block where
  arbitrary =
    do n <- choose (1,100)
       x <- arbitrary
       return (Block n x)
  
  shrink (Block n x) =
    [ Block n' x | n' <- shrink n, n' > 0 ]
    -- ++ [ Block n x' | x' <- shrink x ]

instance Data Block where
  vals (Block n x) = [fromIntegral n,x]
  fill (Block _ _) = \(n:x:_) -> Block (abs (round n)) x

