module Data where

{-
Varying the data-part of test data by means of expressing them as numerical data.
-}

import Test.QuickCheck
import Optimize
import VBool

--------------------------------------------------------------------------------

class Data a where
  vals :: a -> [Double]
  fill :: a -> [Double] -> a

instance Data () where
  vals _   = []
  fill _ _ = ()

instance (Data a, Data b) => Data (a,b) where
  vals (x,y)    = vals x ++ vals y
  fill (x,y) vs = (fill x (take k vs), fill y (drop k vs))
   where
    k = length (vals x)

instance Data Double where
  vals x       = [x]
  fill _ (v:_) = v

instance Data Int where
  vals n       = [fromIntegral n]
  fill _ (v:_) = round v

instance (Data a, Data b) => Data (Either a b) where
  vals (Left x)  = vals x
  vals (Right y) = vals y
  
  fill (Left  x) = Left  . fill x
  fill (Right y) = Right . fill y

instance Data a => Data [a] where
  vals []     = []
  vals (x:xs) = vals (x,xs)
  
  fill []     vs = []
  fill (x:xs) vs = uncurry (:) (fill (x,xs) vs)

--------------------------------------------------------------------------------

data List a = List [a] Int [a] deriving ( Eq, Ord )

list :: Int -> [a] -> List a
list n xs = List (take (n `min` 30) xs) (0 `max` (n `min` 30)) xs

instance Show a => Show (List a) where
  show (List xs n ys) = show xs {- ++ "(" ++ tail (init (show (drop n ys))) ++ ")" -}

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    do xs <- sequence [ arbitrary | i <- [1..30] ]
       n  <- choose (0,30)
       return (list n xs)

  shrink (List _ n xs) =
    [ list k xs | k <- [0..n-1] ] ++
    [ list n (take i xs ++ [x'] ++ drop (i+1) xs)
    | i <- [0..n-1]
    , x' <- shrink (xs!!i)
    ]

instance Data a => Data (List a) where
  vals (List _ n xs)    = vals n ++ vals xs
  fill (List _ n xs) vs = list (fill n (take 1 vs)) (fill xs (drop 1 vs))

--------------------------------------------------------------------------------

forData :: (Show a, Data a) => a -> (a -> VBool) -> Property
forData x h =
  whenFail (do print (fill x ws)) $
    isTrue ans
 where
  (ws,ans) = goal   isFalse
           . giveUp 50
           . take   1000
           . minimize (repeat 15) (vals x)
           $ h . fill x

-- dummy implmentation without NM for comparison
forData0 :: (Show a, Data a) => a -> (a -> VBool) -> Property
forData0 x h =
  whenFail (print x) $ isTrue (h x)

--------------------------------------------------------------------------------


