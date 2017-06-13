module Data where

{-
Varying the data-part of test data by means of expressing them as numerical data.
-}

import Test.QuickCheck
import Numeric.GSL.Minimization
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

forData :: (Show a, Data a) => a -> (a -> VBool) -> Property
forData x h =
  whenFail (do print x'; putStrLn ("(" ++ show k ++ " iterations)")) $
    isTrue (h x')
 where
  vs   = vals x
  p ws = howTrue (h (fill x ws))
  opt  = minimize NMSimplex2 1 100 box p vs
  box  = [ 15.0 | v <- vs ]
  x'   = fill x (fst opt)
  k    = length (lines (show (snd opt))) - 1

-- dummy implmentation without NM for comparison
forData0 :: (Show a, Data a) => a -> (a -> VBool) -> Property
forData0 x h =
  whenFail (print x) $ isTrue (h x)

--------------------------------------------------------------------------------


