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

sig0 = [
  con "0" (0 :: Pos),
  con "1" (1 :: Pos),
  con "+" ((+) :: Pos -> Pos -> Pos),
  con "*" ((*) :: Pos -> Pos -> Pos),
  con "false" (false :: VBool),
  con "true" (true :: VBool),
  -- con "double" (double :: VBool -> VBool),
  -- con "half" (half :: VBool -> VBool),
  con "nt" (nt :: VBool -> VBool),
  monoType (Proxy :: Proxy VBool),
  monoType (Proxy :: Proxy Pos)]

sig1 = [
  con "&&+" ((&&+) :: VBool -> VBool -> VBool),
  con "||+" ((||+) :: VBool -> VBool -> VBool) ]

signew = [
  con "&&^" ((&&^) :: VBool -> VBool -> VBool),
  con "||^" ((||^) :: VBool -> VBool -> VBool) ]

sig2 = [
  con "&&%" ((&&%) :: VBool -> VBool -> VBool),
  con "||%" ((||%) :: VBool -> VBool -> VBool) ]

sig3 =
  con "#+" (\x (Pos y) -> x #+ y)

sig4 =
  con "#" (\x (Pos y) -> x # y)

main = do
  --putStrLn "-- Background --"
  --quickSpec sig0

  --putStrLn "-- Additive &&/|| --"
  --quickSpec [background sig0, signature sig1]

  putStrLn "-- sqrt &&/|| --"
  quickSpec (series [sig0, signew, [sig4]])
