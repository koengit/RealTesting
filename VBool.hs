module VBool where

infix  4 ==%
infixr 3 &&%, &&+
infixr 3 ||%, ||+
infixr 1 ==>%

--------------------------------------------------------------------------------

newtype VBool = V Double -- x not in (-1..1)
 deriving ( Eq, Ord )

instance Show VBool where
  show (V v) = show v

--------------------------------------------------------------------------------

false, true :: VBool
false = V (-1)
true  = V 1

howTrue :: VBool -> Double
howTrue (V v) = v

isTrue, isFalse :: VBool -> Bool
isTrue  (V v) = v >= 1
isFalse (V v) = v >= (-1)

(#) :: VBool -> Double -> VBool
V v # a | a >= 1 = V (v*a)

(#+) :: VBool -> Double -> VBool
V v #+ a
  | a < 0     = error "negative #+"
  | v >= 1    = V (v+a)
  | otherwise = V (v-a)

inv :: VBool -> VBool
inv (V v) = V (big / v)

big :: Double
big = 100000

--------------------------------------------------------------------------------

nt :: VBool -> VBool
nt (V x) = V (-x)

(&&%), (||%) :: VBool -> VBool -> VBool
V x &&% V y = V (x `min` y)
V x ||% V y = V (x `max` y)

(&&+), (||+) :: VBool -> VBool -> VBool
V x &&+ V y
  | signum x == signum y = V (x + y - signum x)
  | otherwise            = V (x `min` y)
x ||+ y = nt (nt x &&+ nt y)      

--------------------------------------------------------------------------------

(==>%) :: VBool -> VBool -> VBool
x ==>% y
  | isTrue x  = y
  | otherwise = nt x #+ big

--------------------------------------------------------------------------------

(>%), (>=%), (<=%), (<%) :: Real a => a -> a -> VBool
x >=% y
  | x >= y    = true  #+ toDouble (x-y)
  | otherwise = false #+ toDouble (y-x)
x >%  y = nt (x <=% y)
x <%  y = y >% x
x <=% y = y >=% x

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational

--------------------------------------------------------------------------------

class VEq a where
  (==%) :: a -> a -> VBool

instance VEq Double where
  x ==% y
    | x == y    = true
    | otherwise = false #+ abs (x-y)

instance VEq Int where
  x ==% y
    | x == y    = true
    | otherwise = false #+ fromIntegral (abs (x-y))

instance VEq Bool where
  x ==% y = if x == y then true else false

instance VEq VBool where
  x ==% y
    | isTrue x == isTrue y = true  #+ (big / (0.1+d))
    | otherwise            = false #+ d
   where
    d = abs (howTrue x - howTrue y)

instance VEq () where
  _ ==% _ = true

instance (VEq a, VEq b) => VEq (a,b) where
  (a1,a2) ==% (b1,b2) = a1 ==% b1 &&+ a2 ==% b2

instance VEq a => VEq [a] where
  []     ==% []     = true
  (a:as) ==% (b:bs) = (a==%b) &&+ (as ==% bs)
  as     ==% bs     = false #+ fromIntegral (length (as++bs))

--------------------------------------------------------------------------------

