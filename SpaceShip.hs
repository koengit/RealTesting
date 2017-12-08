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
  map (>=%100) xs' `thenn`
    (map (<=%(-100)) xs' `thenn` map (>=%100) xs') 
 where
  xs' = take 6000 xs

thenn :: [VBool] -> [VBool] -> [VBool]
as `thenn` bs = scanr1 (||+) (zipWith (&&+) as (scanr1 (||+) bs))

prop_Ship :: ([Block],[Block]) -> VBool
prop_Ship (blks,blks2) =
  foldr (&&+) true [ (-1) <=% a &&+ a <=% 1 | a <- accs ] ==>%
    nt (checks (ship accs))
 where
  accs = concat [ replicate n x | Block n x <- blks++blks2 ]

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
    [ Block n' x | n' <- shrink n, n' > 0 ] ++
    [ Block n x' | x' <- shrink x ]

instance Data Block where
  vals (Block n x) = [x]
  fill (Block n _) = \(x:_) -> Block n x

