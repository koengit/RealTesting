module Val where

import qualified Data.Map as M
import Data.List( sort, sortBy )
import Data.Ord
import VBool
import Data
import Test.QuickCheck
import Badness

newtype Val a = Val [(a,VBool)]
 deriving ( Eq, Ord, Show )

val :: a -> Val a
val x = Val [(x,true)]

class Choice a where
  ifThenElse :: VBool -> a -> a -> a

instance Choice Double where
  ifThenElse c x y = if isTrue c then x else y -- loses VBool info

instance Choice VBool where
  ifThenElse c x y = (c &&+ x) ||+ (nt c &&+ y)

instance Ord a => Choice (Val a) where
  ifThenElse c (Val xs) (Val ys) = Val (go xs ys)
   where
    go xs ys
      | null xs && null ys                  = []
      | null ys || (not (null xs) && x < y) = (x, c    &&+ vx)         : go xs' ys
      | null xs || (not (null ys) && y < x) = (y, nt c &&+ vy)         : go xs ys'
      | otherwise                           = (x, ifThenElse c vx vy)  : go xs' ys'
     where
      (x,vx):xs' = xs
      (y,vy):ys' = ys

mapVal :: Ord b => (a->b) -> Val a -> Val b
mapVal f (Val xs) =
  Val $ M.toList $ M.fromListWith (||+)
  [ (f x, a)
  | (x,a) <- xs
  ]

zipVal :: Ord c => (a->b->c) -> Val a -> Val b -> Val c
zipVal f (Val xs) (Val ys) =
  Val $ M.toList $ M.fromListWith (||+)
  [ (f x y, a &&+ b)
  | (x,a) <- xs
  , (y,b) <- ys
  ]

instance (Ord a, Num a) => Num (Val a) where
  (+)         = zipVal (+)
  (-)         = zipVal (-)
  (*)         = zipVal (*)
  abs         = mapVal abs
  negate      = mapVal negate
  signum      = mapVal signum
  fromInteger = val . fromInteger

instance (Ord a, Fractional a) => Fractional (Val a) where
  (/)          = zipVal (/)
  recip        = mapVal recip
  fromRational = val . fromRational

smash :: Val VBool -> VBool
smash (Val xs) = foldr1 (||+) [ b &&+ x | (x,b) <- xs ]

class VCompare a where
  (==?), (/=?), (>?), (>=?), (<?), (<=?) :: a -> a -> VBool
  
instance VCompare Double where
  (==?) = (==%)
  (/=?) = \x y -> nt (x ==% y)
  (>?)  = (>%)
  (>=?) = (>=%)
  (<?)  = (<%)
  (<=?) = (<=%)
  
instance VCompare a => VCompare (Val a) where
  x ==? y = smash (zipVal (==?) x y)
  x /=? y = smash (zipVal (/=?) x y)
  x >?  y = smash (zipVal (>?)  x y)
  x >=? y = smash (zipVal (>=?) x y)
  x <?  y = smash (zipVal (<?)  x y)
  x <=? y = smash (zipVal (<=?) x y)

forget :: Ord a => Val a -> Val a
forget (Val xs) = Val (sort (take 100 (sortBy (comparing worst) xs)))
 where
  worst (_,a) = -howTrue a

--------------------------------------------------------------------------------

f :: (Num a, VCompare a, Choice a) => a -> a
f x =
  ifThenElse (x <? a) 10
    (ifThenElse (x >? (a+d)) 20
      (-1))

g :: (Fractional a, VCompare a, Choice a) => a -> a
g x =
  ifThenElse (x <? a) (1 + (100 / (a-x)))
    (ifThenElse (x >? (a+d)) (1 + (100 / (x-(a+d))))
      (-1))

a, d :: Num a => a
a = 202
d = 2

prop_Basic f x =
  withBadness $
  let (y,b) = forData x (\x -> f (x :: Double) >=? 0) in
    whenFail (print y) (isTrue b)

prop_Val f x =
  withBadness $
  let (y,b) = forData x (\x -> f (val x) >=? 0) in
    whenFail (print y) (isTrue b)

