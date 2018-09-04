{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Poly where

import VBool
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.Maybe
import Optimize
import GHC.Generics
import Data.List
import Control.Monad
import Control.Arrow

newtype Var = Var Int deriving (Eq, Ord, CoArbitrary)
instance Arbitrary Var where
  arbitrary = elements vars
  shrink x = takeWhile (/= x) vars

instance Show Var where
  show (Var x) = "x" ++ show x

vars :: [Var]
vars = map Var [1..20]

data Term = Term Double [(Var, Double, Double)]
  deriving (Eq, Ord, Generic)

instance Show Term where
  show (Term x xs) =
    show x ++ " " ++ intercalate " " (map showVar (sort xs))
    where
     showVar (x, a, k) = "(" ++ show x ++ "+" ++ show a ++ ")^" ++ show k

instance Arbitrary Term where
  arbitrary = Term <$> arbitrary <*> resize 4 (listOf arbVar)
  -- Add smaller to QuickCheck?
    where
      smaller gen = sized (\n -> resize (n `div` 2) gen)
      larger gen = sized (\n -> resize (n * 2) gen)
      arbVar = liftM3 (,,) arbitrary (arbitrary `suchThat` (\x -> x >= -1 && x < 1)) (frequency [(5, resize 5 arbitrary `suchThat` (>= 0)), (1, return 0)])
  shrink = filter p . genericShrink
    where p (Term _ xs) = and [k > 0 | (_, _, k) <- xs]

newtype Poly = Poly [Term]
  deriving (Eq, Ord, Arbitrary)

-- instance Arbitrary Poly where
--   arbitrary = Poly <$> vector 10

instance Show Poly where
  show (Poly ts) = intercalate " + " (map show ts)

newtype Valuation = Valuation [(Var, Double)]
  deriving (Eq, Ord, Show)

instance Arbitrary Valuation where
  arbitrary = do
    val <- resize 2 arbitrary
    return (Valuation [(x, val x) | x <- vars])

-- Evaluate a polynomial
eval :: Poly -> Valuation -> Double
eval (Poly ts) (Valuation xs) = sum (map evalTerm ts)
  where
    evalTerm (Term a as) =
      a * product (map evalVar as)
    evalVar (x, a, k) = (fromJust (lookup x xs))**k

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
    d = bound / 2
    h xs = prop op f (Valuation (zipWith twiddle xs0 xs))
    twiddle (x, _) y = (x, y)

nelderMeads :: (VBool -> VBool -> VBool) -> (Valuation -> (Double, Double)) -> [Valuation] -> Double
nelderMeads op f xss =
  minimum (map (nelderMead op f) xss)

genRoot :: Gen (Valuation -> Double)
genRoot = do
  poly <- arbitrary
  return (\xs@(Valuation ys) -> if all (> -bound) (map snd ys) && all (< bound) (map snd ys) then fix (abs (eval poly (Valuation (map (id *** (+ bound)) ys)))) else 1000000000 * badness ys)
  where
    badness ys = sum [ z | (_, y) <- ys, let z = abs y - bound, z > 0 ]
    fix x = if isNaN x then error "NaN" else x

bound = 2

-- Generate an always-positive function using abs
genTrue :: Gen (Valuation -> Double)
genTrue = do
  val <- genRoot
  return (\xs -> val xs + 1)

-- Generate a probably-not-always-positive function
genFalse :: Gen (Valuation -> Double)
genFalse = oneof [
  --eval <$> arbitrary,
  do
    val <- genRoot
    -- Assumption: the original was falsifiable
    return (\xs -> val xs - 1)]

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
      xss <- generate (vector 100)
      if any (< 0) (map (prop op0 f) xss) then do
        loop counts total
       else do
        let results = [nelderMeads op f (take 10 xss) | (_, op) <- ops]
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
-- interpretation for the upper-right quadrant
ors :: [(String, Inf -> Inf -> Inf)] -> [(String, VBool -> VBool -> VBool)]
ors ops =
  [(name, disjunction op op) | (name, op) <- ops]

-- Build a set of VBool operators for variants of &&, given an
-- interpretation for the upper-right quadrant
ands :: [(String, Inf -> Inf -> Inf)] -> [(String, VBool -> VBool -> VBool)]
ands ops =
  [(name, conjunction op op) | (name, op) <- ops]

-- Test some variants of ||
testOrs =
  test (ors [("min", minInf), ("max", maxInf), ("plus", plusInf), ("par", parInf), ("dist", distInf), ("rand", randInf)]) genOr

-- Test some variants of &&
testAnds =
  test (ands [("min", minInf), ("max", maxInf), ("plus", plusInf), ("par", parInf), ("dist", distInf), ("rand", randInf)]) genAnd

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

randInf :: Inf -> Inf -> Inf
randInf = unGen arb (mkQCGen 1234) 100
  where
    arb = do
      f <- arbitrary
      return $ \x y -> absy (f x y)
    absy Infinite = Infinite
    absy (Finite x) = Finite (abs x)

plot :: Gen (Valuation -> (Double, Double)) -> (VBool -> VBool -> VBool) -> IO ()
plot gen op = do
  tests <-
    generate $ vectorOf 1 $ do
      poly <- gen
      points <- vectorOf 1 (arbitrary `suchThat` (\p -> fst (poly p) > 0 && snd (poly p) > 0))
      return (poly, points)
  writeFile "data" $ unlines $ do
    (poly, points) <- tests
    point@(Valuation xs) <- points
    (x, _, _) <- map head $ group $ giveUp 100 $ nelderMead' op poly point
    let val = Valuation (zip (map fst xs) x)
    let (y, z) = poly val
    guard (y < 1000000000 && z < 1000000000)
    return (unwords (map show [y, z]))
