{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Poly where

import VBool
import Test.QuickCheck
import Data.Maybe
import Optimize
import GHC.Generics
import Data.List
import Control.Monad
import Control.Arrow

newtype Var = Var Int deriving (Eq, Ord, Show, CoArbitrary)
instance Arbitrary Var where
  arbitrary = elements vars
  shrink x = takeWhile (/= x) vars

-- instance Show Var where
--   show (Var x) = "x" ++ show x

vars :: [Var]
vars = map Var [1..10]

data Term = Term Double [Var]
  deriving (Eq, Ord, Show, Generic)

-- instance Show Term where
--   show (Term x xs) =
--     show x ++ " " ++ concatMap show xs

instance Arbitrary Term where
  arbitrary = Term <$> arbitrary <*> smaller arbitrary
  -- Add smaller to QuickCheck?
    where
      smaller gen = sized (\n -> resize (n `div` 2) gen)
  shrink = genericShrink

newtype Poly = Poly [Term]
  deriving (Eq, Ord, Show, Arbitrary)

-- instance Arbitrary Poly where
--   arbitrary = Poly <$> vector 10

-- instance Show Poly where
--   show (Poly ts) = intercalate " + " (map show ts)

newtype Valuation = Valuation [(Var, Double)]
  deriving (Eq, Ord, Show)

instance Arbitrary Valuation where
  arbitrary = do
    val <- arbitrary
    return (Valuation [(x, val x) | x <- vars])

-- Evaluate a polynomial
eval :: Poly -> Valuation -> Double
eval (Poly ts) (Valuation xs) = sum (map evalTerm ts)
  where
    evalTerm (Term a as) =
      a * product (map evalVar as)
    evalVar x = fromJust (lookup x xs)

-- Apply a VBool operator to a pair of polynomials
prop :: (VBool -> VBool -> VBool) -> (Valuation -> (Double, Double)) -> Valuation -> Double
prop op f xs =
  howTrue ((fst (f xs) >% 0) `op` (snd (f xs) >% 0))

-- Same as above, but using Nelder-Mead
nelderMead :: (VBool -> VBool -> VBool) -> (Valuation -> (Double, Double)) -> Valuation -> Double
nelderMead op f xs =
  snd . goal p . giveUp k . nelderMead' op f $ xs
  where
    p = (< 0)
    k = 100
    
nelderMead' :: (VBool -> VBool -> VBool) -> (Valuation -> (Double, Double)) -> Valuation -> [(Point, Double, Double)]
nelderMead' op f (Valuation xs0) =
  take n . minimize (repeat d) (map snd xs0) $ h
  where
    n = 1000
    d = 1000
    h xs = prop op f (Valuation (zipWith twiddle xs0 xs))
    twiddle (x, _) y = (x, y)

-- Generate an always-positive function using abs
genTrue :: Gen (Valuation -> Double)
genTrue = do
  poly <- arbitrary
  return (\xs -> abs (eval poly xs) + 1)

-- Generate a probably-not-always-positive function
genFalse :: Gen (Valuation -> Double)
genFalse = oneof [
  eval <$> arbitrary,
  do
    poly <- arbitrary
    -- Assumption: the original was falsifiable
    return (\xs -> abs (eval poly xs) - 1)]

-- Generate a pair of functions for testing && (one of them should not
-- always be positive)
genAnd :: Gen (Valuation -> (Double, Double))
genAnd =
  oneof [
    liftM2 (&&&) genTrue genFalse,
    liftM2 (&&&) genFalse genTrue,
    liftM2 (&&&) genFalse genFalse]

-- Generate a pair of functions for testing || (both of them should
-- not always be positive)
genOr :: Gen (Valuation -> (Double, Double))
genOr = liftM2 (&&&) genFalse genFalse

-- The main test function
test :: [(String, VBool -> VBool -> VBool)] -> Gen (Valuation -> (Double, Double)) -> IO ()
test ops@((_, op0):_) gen = do
  header
  loop (repeat 0) 0
  where
    loop counts total = do
      f <- generate gen
      xs <- generate arbitrary
      if prop op0 f xs < 0 then do
        loop counts total
       else do
        let results = [nelderMead op f xs | (_, op) <- ops]
        if all (< 0) results || all (>= 0) results then do
          loop counts total
         else do
          let
            counts' = zipWith (+) (map fromEnum (map (< 0) results)) counts
            total' = total+1
          when (total' `mod` 25 == 1 && total' /= 1) $ do
            putStrLn ""
            header
          putStrLn (tabulate (show total':map (percent total') counts'))
          loop counts' total'

    header = putStrLn (tabulate ("":map fst ops))

    tabulate [] = ""
    tabulate (xs:xss) =
      replicate (7 - length xs) ' ' ++ xs ++ tabulate xss

    percent n k = show (100 * k `div` n) ++ "%"

-- Build a set of VBool operators for variants of ||, given an
-- interpretation for the upper-right quadrant and for the
-- upper-left/lower-right quadrants
ors :: [(String, Inf -> Inf -> Inf, Inf -> Inf -> Inf)] -> [(String, VBool -> VBool -> VBool)]
ors ops =
  [(name, disjunction op1 op1 op2) | (name, op1, op2) <- ops]

-- Same as above but using standard interpretation for
-- upper-left/lower-right quadrants
ors' :: [(String, Inf -> Inf -> Inf)] -> [(String, VBool -> VBool -> VBool)]
ors' ops =
  ors [(name, op, const) | (name, op) <- ops]

-- Build a set of VBool operators for variants of &&, given an
-- interpretation for the upper-right quadrant
ands :: [(String, Inf -> Inf -> Inf)] -> [(String, VBool -> VBool -> VBool)]
ands ops =
  [(name, conjunction op op const) | (name, op) <- ops]

-- Test some variants of ||
testOrs =
  test (ors' [("min", minInf), ("max", maxInf), ("plus", plusInf), ("par", parInf), ("dist", distInf)]) genOr

-- Test some variants of &&
testAnds =
  test (ands [("min", minInf), ("max", maxInf), ("plus", plusInf), ("par", parInf), ("dist", distInf)]) genAnd

-- Silly interpretation for upper-left/lower-right quadrants
silly :: Inf -> Inf -> Inf
silly Infinite Infinite = Finite 100
silly Infinite _ = Infinite
silly _ Infinite = Finite 0
silly (Finite x) (Finite y) = Finite (x * (a/(y+a))**b)
  where
    a = 1
    b = 0.5

-- Minimum
minInf (Finite x) (Finite y) = Finite (min x y)
minInf (Finite x) Infinite = Finite x
minInf Infinite (Finite x) = Finite x
minInf Infinite Infinite = Infinite

-- Maximum
maxInf (Finite x) (Finite y) = Finite (max x y)
maxInf _ _ = Infinite
