{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VBool where

import Test.QuickCheck

import Shape

padTo :: Int -> a -> [a] -> [a]
padTo n a as = as ++ replicate (n - length as) a

distance :: [Double] -> [Double] -> Double
distance xs ys = sum ((**2) <$> zipWith (-) xs' ys')
  where
    longest = max (length xs) (length ys)
    xs' = padTo longest 0 xs
    ys' = padTo longest 0 ys

newtype VBool = VBool {unVBool :: Double}
  deriving (Show, Num, Floating, Fractional, Ord, Eq)

instance Testable VBool where
  property = property . toBool

(==%) :: HasShape a => a -> a -> VBool
a ==% b = if (shapeOf a == shapeOf b) then
            (VBool (1 - distance (measure a) (measure b)))
          else
            (VBool (0 - distance (measure a) (measure b)))

(%==>) :: VBool -> VBool -> VBool
a %==> b = nt a ||+ b

(<=%) :: (Ord a, HasShape a) => a -> a -> VBool
a <=% b
  | a <= b    = VBool (1 + distance (measure a) (measure b))
  | otherwise = VBool (negate $ 1 + distance (measure a) (measure b))

(<%) :: (Ord a, HasShape a) => a -> a -> VBool
a <% b = a <=% b &&+ nt (a ==% b)

(&&+) :: VBool -> VBool -> VBool
(VBool l) &&+ (VBool r)
  | l > 0 && r > 0 = VBool $ l + r
  | l < 0 && r < 0 = VBool $ l + r
  | otherwise      = VBool $ min l r

(||+) :: VBool -> VBool -> VBool
(VBool l) ||+ (VBool r)
  | l > 0 && r > 0 = VBool $ l + r
  | l < 0 && r < 0 = VBool $ l + r 
  | otherwise      = VBool $ max l r

nt :: VBool -> VBool
nt = negate

true, false :: VBool
true  = 1
false = -1

orP :: [VBool] -> VBool
orP = foldr (||+) false

andP :: [VBool] -> VBool
andP = foldr (&&+) true

toBool :: VBool -> Bool
toBool = (0<)
