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

the :: Val a -> a
the (Val xs) = head [ x | (x,v) <- xs, isTrue v ]

mkVal :: Ord a => [(a,VBool)] -> Val a
mkVal = Val . M.toList . M.fromListWith (||+)

vbool :: VBool -> Val Bool
vbool v =
  Val [ (False, nt v)
      , (True,  v)
      ]

class Choice a where
  ifThenElse :: Val Bool -> a -> a -> a

instance Choice Double where
  ifThenElse c x y =
    if the c then x else y -- loses VBool info

instance Ord a => Choice (Val a) where
  ifThenElse (Val cs) (Val xs) (Val ys) =
    mkVal
    [ (z,cv &&+ zv)
    | (c,cv) <- cs
    , (z,zv) <- if c then xs else ys
    ]

mapVal :: Ord b => (a->b) -> Val a -> Val b
mapVal f (Val xs) =
  mkVal
  [ (f x, a)
  | (x,a) <- xs
  ]

liftVal :: Ord c => (a->b->c) -> Val a -> Val b -> Val c
liftVal f (Val xs) (Val ys) =
  mkVal
  [ (f x y, a &&+ b)
  | (x,a) <- xs
  , (y,b) <- ys
  ]

instance (Ord a, Num a) => Num (Val a) where
  (+)         = liftVal (+)
  (-)         = liftVal (-)
  (*)         = liftVal (*)
  abs         = mapVal abs
  negate      = mapVal negate
  signum      = mapVal signum
  fromInteger = val . fromInteger

instance (Ord a, Fractional a) => Fractional (Val a) where
  (/)          = liftVal (/)
  recip        = mapVal recip
  fromRational = val . fromRational

class VCompare a where
  (==?), (/=?), (>?), (>=?), (<?), (<=?) :: a -> a -> Val Bool

instance VCompare Double where
  (==?) = compDouble (==%)
  (/=?) = compDouble (\x y -> nt (x ==% y))
  (>?)  = compDouble (>%)
  (>=?) = compDouble (>=%)
  (<?)  = compDouble (<%)
  (<=?) = compDouble (<=%)

compDouble op x y =
  vbool (x `op` y)

instance VCompare a => VCompare (Val a) where
  (==?) = compVal (==?)
  (/=?) = compVal (/=?)
  (>?)  = compVal (>?)
  (>=?) = compVal (>=?)
  (<?)  = compVal (<?)
  (<=?) = compVal (<=?)

compVal op x y =
  smash (liftVal op x y)

smash :: Ord a => Val (Val a) -> Val a
smash (Val vs) =
  mkVal
  [ (w, a &&+ b)
  | (Val ws,a) <- vs
  , (w     ,b) <- ws
  ]

--------------------------------------------------------------------------------

forget :: Ord a => Val a -> Val a
forget (Val xs) = Val (sort (take 100 (sortBy (comparing worst) xs)))
 where
  worst (_,a) = -howTrue a

propVal :: Val Bool -> VBool
propVal (Val bs) = foldr1 (&&+) [ if b then v else nt v | (b,v) <- bs ]

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
  let (y,b) = forData x (\x -> propVal (f (x :: Double) >=? 0)) in
    whenFail (print y) (isTrue b)

prop_Val f x =
  withBadness $
  let (y,b) = forData x (\x -> propVal (f (val x) >=? 0)) in
    whenFail (print y) (isTrue b)

