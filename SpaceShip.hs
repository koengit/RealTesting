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
checks xs = check 500 [(>100),(<(-100)),(>100)] xs
 where
  check 0 _  _  = false
  check _ [] _  = true
  check _ _  [] = false
  check n (p:ps) (x:xs)
    | p x       = check (n-1) ps xs
    | otherwise = check (n-1) (p:ps) xs

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

