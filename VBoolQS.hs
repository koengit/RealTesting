-- NOTE: before running this, change VBool module to use Rational instead of Double!

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--module VBoolQS where

import QuickSpec
import Test.QuickCheck
import VBool

newtype Pos = Pos Rational deriving (Eq, Ord, Num)

instance Arbitrary Pos where
  arbitrary = do
    NonNegative x <- arbitrary
    return (Pos x)

sig0 =
  signature {
    constants = [
      constant "0" (0 :: Pos),
      constant "1" (1 :: Pos),
      constant "+" ((+) :: Pos -> Pos -> Pos),
      constant "*" ((*) :: Pos -> Pos -> Pos),
      constant "false" (false :: VBool),
      constant "true" (true :: VBool),
      -- constant "double" (double :: VBool -> VBool),
      -- constant "half" (half :: VBool -> VBool),
      constant "nt" (nt :: VBool -> VBool)],
    instances =
      [baseType (undefined :: VBool),
       baseType (undefined :: Pos)] }

sig1 =
  signature {
    constants = [
      constant "&&+" ((&&+) :: VBool -> VBool -> VBool),
      constant "||+" ((||+) :: VBool -> VBool -> VBool) ]}

sig2 =
  signature {
    constants = [
      constant "&&%" ((&&%) :: VBool -> VBool -> VBool),
      constant "||%" ((||%) :: VBool -> VBool -> VBool) ]}

sig3 =
  signature {
    constants = [
      constant "#+" (\x (Pos y) -> x #+ y) ]}
          
sig4 =
  signature {
    constants = [
      constant "#" (\x (Pos y) -> x # y) ]}
          
main = do
  putStrLn "-- Background --"
  thy0 <- quickSpec sig0

  putStrLn "-- Additive &&/|| --"
  thy1 <- quickSpec (thy0 `mappend` sig1)

  putStrLn "-- Min/max &&/|| --"
  thy2 <- quickSpec (thy0 `mappend` sig2)

  putStrLn "-- Both &&/|| --"
  thy <- quickSpec (thy1 `mappend` thy2)

  putStrLn "-- Additive # --"
  thy3 <- quickSpec (thy `mappend` sig3)

  putStrLn "-- Multiplicative # --"
  thy4 <- quickSpec (thy `mappend` sig4)

  putStrLn "-- Everything --"
  quickSpec (thy3 `mappend` thy4)
