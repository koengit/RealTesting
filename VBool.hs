{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module VBool where

import Badness
import Test.QuickCheck
import Data.List
import Data.Reflection

infix  4 ==%
infixr 3 &&%, &&+
infixr 3 ||%, ||+
infixr 1 ==>%

--------------------------------------------------------------------------------

data VBool = VFalse Inf | VTrue Inf -- x non-negative
 deriving (Eq, Ord)

instance Show VBool where
  show (VFalse v) = "false " ++ show (fromInf v)
  show (VTrue v) = "true " ++ show (fromInf v)

instance Arbitrary VBool where
  arbitrary = do
    x <- arbitrary
    val <- elements [VFalse, VTrue]
    return (val x)

--------------------------------------------------------------------------------

-- A nonzero real number which can also be infinite.
data Inf = Finite Double | Infinite
  deriving (Eq, Ord)

plusInf :: Inf -> Inf -> Inf
plusInf (Finite x) (Finite y) = Finite (x+y)
plusInf _ _ = Infinite

-- parInf x y = 1/(1/x + 1/y)
parInf :: Inf -> Inf -> Inf
parInf (Finite x) (Finite y)
  | x /= 0, y /= 0 =
    Finite (recip (recip x + recip y))
-- 1/(1/x + 1/y) -> y as x -> 0
parInf (Finite 0) (Finite _) = Finite 0
parInf (Finite _) (Finite 0) = Finite 0
-- 1/infty = 0
parInf (Finite x) Infinite = Finite x
parInf Infinite (Finite y) = Finite y
parInf Infinite Infinite = Infinite

scaleInf :: Real a => Inf -> a -> Inf
scaleInf (Finite x) a = Finite (x*fromRational (toRational a))
scaleInf Infinite _ = Infinite

fromInf :: Inf -> Double
fromInf (Finite x) = x
fromInf Infinite = 1/0

toInf :: Real a => a -> Inf
toInf = Finite . fromRational . toRational

instance Arbitrary Inf where
  arbitrary =
    oneof [
      return Infinite,
      do
        NonNegative x <- arbitrary
        return (Finite x) ]

--------------------------------------------------------------------------------

false, true :: VBool
false = VFalse Infinite
true  = VTrue Infinite

bad, good :: Real a => a -> VBool
bad x = VFalse (toInf x)
good x = VTrue (toInf x)

howTrue :: VBool -> Double
howTrue (VFalse x) = -fromInf x
howTrue (VTrue x) = 1 + fromInf x

isTrue, isFalse :: VBool -> Bool
isTrue (VFalse _) = False
isTrue (VTrue _) = True
isFalse = not . isTrue

(#) :: Real a => VBool -> a -> VBool
_ # a | a < 0 = error "negative #"
VFalse x # a = VFalse (scaleInf x a)
-- XXX should this use par?
VTrue x # a = VTrue (scaleInf x a)

(#+) :: Real a => VBool -> a -> VBool
_ #+ a | a < 0 = error "negative #+"
VFalse x #+ a = VFalse (plusInf x (toInf a))
-- XXX should this be par?
VTrue x #+ a = VTrue (plusInf x (toInf a))

--------------------------------------------------------------------------------

nt :: VBool -> VBool
nt (VFalse x) = VTrue x
nt (VTrue x) = VFalse x

(&&%) :: VBool -> VBool -> VBool
VFalse x &&% VFalse y = VFalse (x `max` y)
VTrue x &&% VTrue y = VTrue (x `min` y)
VFalse x &&% VTrue _ = VFalse x
VTrue _ &&% VFalse x = VFalse x

(&&+) :: VBool -> VBool -> VBool
VFalse x &&+ VFalse y = VFalse (plusInf x y)
VTrue x &&+ VTrue y = VTrue (parInf x y)
VTrue _x &&+ VFalse y = VFalse y
VFalse x &&+ VTrue _y = VFalse x

(||%), (||+) :: VBool -> VBool -> VBool
x ||% y = nt (nt x &&% nt y)      
x ||+ y = nt (nt x &&+ nt y)      

conj :: [VBool] -> VBool
conj = foldl' (&&+) true

disj :: [VBool] -> VBool
disj = foldl' (||+) false

--------------------------------------------------------------------------------

big :: Double
big = 100000

(==>%) :: VBool -> VBool -> VBool
x ==>% y
  | isTrue x  = y
  | otherwise = nt x #+ big

--------------------------------------------------------------------------------

(>%), (>=%), (<=%), (<%) :: Real a => a -> a -> VBool
x >=% y
  | x >= y    = good (x-y)
  | otherwise = bad (y-x)
x >%  y = nt (x <=% y)
x <%  y = y >% x
x <=% y = y >=% x

--------------------------------------------------------------------------------

class VEq a where
  (==%) :: a -> a -> VBool

instance VEq Double where
  x ==% y
    | x == y    = true
    | otherwise = bad (abs (x-y))

instance VEq Int where
  x ==% y
    | x == y    = true
    | otherwise = bad (fromIntegral (abs (x-y)))

instance VEq Bool where
  x ==% y = if x == y then true else false

instance VEq VBool where
  x ==% y
    | isTrue x == isTrue y = good (big / (0.1+d))
    | otherwise            = bad d
   where
    d = abs (howTrue x - howTrue y)

instance VEq () where
  _ ==% _ = true

instance (VEq a, VEq b) => VEq (a,b) where
  (a1,a2) ==% (b1,b2) = a1 ==% b1 &&+ a2 ==% b2

instance VEq a => VEq [a] where
  []     ==% []     = true
  (a:as) ==% (b:bs) = (a==%b) &&+ (as ==% bs)
  as     ==% bs     = bad (fromIntegral (length (as++bs)))

--------------------------------------------------------------------------------

instance Given Badness => Testable VBool where
  --property x = property (isTrue x)
  property x = badness given (- howTrue x) (isTrue x)
