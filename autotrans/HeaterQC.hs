{-# LANGUAGE FlexibleContexts, DeriveGeneric, StandaloneDeriving #-}
module Main where

import Zelus hiding (start)
import VBool
import Plot
import Badness
import HeaterModel

import Data.Reflection
import Data.List( nub, sortOn, foldl' )
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import GHC.Generics
import System.Random

--------------------------------------------------------------------------------
-- heater + controller

type Level = Double -- pump level
type Temp  = Double -- temperature

choose' :: (Ord a, Random a, Fractional a) => (a, a) -> Gen a
choose' (x, y) =
  clamp <$> choose (x - delta, y + delta)
  where
    delta = (y - x) / 10
    clamp z
      | z < x = x
      | z > y = y
      | otherwise = z

deriving instance Generic Input
instance Arbitrary Input where
  arbitrary = do
    temp <- choose' (10, 30)
    return Input{goal = temp}
  shrink = filter (ok . goal) . genericShrink
    where
      ok x = 10 <= x && x <= 30

data Line = Line { start :: Input, end :: Input } deriving (Show, Generic)

instance Arbitrary Line where
  arbitrary =
    oneof [
      -- Line <$> arbitrary <*> arbitrary,
      (\x -> Line x x) <$> arbitrary]
  shrink (Line x y)
    | x == y = (\x -> Line x x) <$> shrink x
    | otherwise = Line x x:Line y y:Line (bin avg x y) (bin avg x y):genericShrink (Line x y)
    where
      avg x y = (x+y)/2

bin :: (Double -> Double -> Double) -> Input -> Input -> Input
bin op x y =
  Input {
    goal = goal x `op` goal y }

diff :: Line -> Input
diff line = bin (-) (end line) (start line)

newtype Inputs = Inputs [(Double, Line)] deriving Show

instance Arbitrary Inputs where
  arbitrary = do
    duration <- sized $ \n -> choose (0, 3*fromIntegral n)
    cut duration <$> Inputs <$>
      infiniteListOf (do
        sqrtduration <- choose (0, sqrt duration)
        let duration = sqrtduration^2
        line <- arbitrary
        return (duration, line))
  shrink (Inputs inps) =
    Inputs <$>
      merge inps ++
      genericShrink inps
    where
      merge (x:y:xs) =
        merging x y xs ++
        map (x:) (merge (y:xs))
      merge _ = []
      merging (t,x) (u,y) xs =
        [(t+u, x):xs,
         (t+u, y):xs,
         (t+u, Line (start x) (end y)):xs]

cut :: Double -> Inputs -> Inputs
cut x (Inputs inps) = Inputs (aux x inps)
  where
    aux x _ | x <= 0 = []
    aux _ [] = []
    aux x ((y, line):inps)
      | x <= y = [(x, line)]
      | otherwise = (y, line):aux (x-y) inps

sampleInputs :: Double -> Inputs -> [Input]
sampleInputs delta (Inputs xs) =
  concat (zipWith interpolate times lines)
  where
    times = map truncate (map (/ delta) lengths)
    (lengths, lines) = unzip xs
    interpolate n line =
      map (interp1 n line) [0..n-1]
    interp1 n line i =
      bin (\a b -> a + x * b) (start line) (diff line)
      where
        x = fromIntegral i / fromIntegral n

withTestCase :: Testable prop => (Double -> [(Input, Output)] -> prop) -> Property
withTestCase prop = property $ \inps -> ioProperty $ do
  delta <- stepSize
  let inputs = sampleInputs delta inps
      outputs = runModel inputs
      graph xs = (map (* delta) [0..], xs)
  return $ prop delta (zip inputs outputs)

--------------------------------------------------------------------------------
-- properties

main = quickCheckWith stdArgs{maxSuccess = 10000} (withBadness prop_ReactFast)

--------------------------------------------------------------------------------

for' n = foldr1 (&&+) . take n

prop_ReactFast :: Given Badness => Property
prop_ReactFast =
  withTestCase $ \delta test ->
  ioProperty $ do
  let
    (input, output) = unzip test
  
    goalTemp = map goal input
    roomTemp = map temperature output
    errTemp  = abs (goalTemp - roomTemp)
  
    stableFor = n
     where
      --n = 1 |> (goalTemp ==? pre goalTemp ? (pre n+1,1))
      n = integ (1 `in1t` 1 `reset` (1 `when` (goalTemp /=? pre goalTemp)))

    ok = (stableFor >=? val (truncate (50 / delta))) ? (zipWith (<=%) errTemp (val 1), good <$> stableFor `min` 0)
  
  return $
    whenFail (plot "failed" (length test)
            [ ("ok", graph (map howTrue ok))
            , ("goal",graph goalTemp)
            , ("room",graph roomTemp)
            ]) $
      conj' (take (length test) ok)
