{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module AutoTrans where

import AutoTransModel
import Plot
import Test.QuickCheck
import GHC.Generics
import VBool
import Badness
import Data.List

deriving instance Generic Input
instance Arbitrary Input where
  arbitrary = do
    throttle <- choose (0, 100)
    brake <- choose (0, 100)
    return Input{throttle = throttle, brake = brake}
  shrink = filter ok . genericShrink
    where
      ok input = ok1 (throttle input) && ok1 (brake input)
      ok1 x = 0 <= x && x <= 100

newtype Inputs = Inputs [(Double, Input)] deriving Show

instance Arbitrary Inputs where
  arbitrary =
    Inputs <$>
      listOf1 (do
        duration <- choose (0, 20)
        input <- arbitrary
        return (duration, input))
  shrink (Inputs inps) = Inputs <$> shrinkList (const []) inps ++ shr inps ++ genericShrink inps
    where
      shr (x:y:xs) =
        merging x y xs ++
        map (x:) (shr (y:xs))
      shr _ = []
      merging (t,x) (u,y) xs =
        [(t+u, x):xs,
         (t+u, y):xs]

sampleInputs :: Double -> Inputs -> [Input]
sampleInputs delta (Inputs xs) =
  concat (zipWith replicate times inputs)
  where
    times = map truncate (map (/ delta) lengths)
    (lengths, inputs) = unzip xs

withTestCase :: Testable prop => (Double -> [(Input, Output)] -> prop) -> Property
withTestCase prop = property $ \inps -> ioProperty $ do
  delta <- stepSize
  let inputs = sampleInputs delta inps
  outputs <- runModel inputs
  let graph xs = (map (* delta) [0..], xs)
  return $
    whenFail (plot "failed" (ceiling (fromIntegral (length inputs) / delta))
      [("throttle", graph (map throttle inputs)),
       ("brake", graph (map brake inputs)),
       ("speed", graph (map speed outputs)),
       ("rpm", graph (map rpm outputs)),
       ("gear", graph (map gear outputs))]) $
      prop delta (zip inputs outputs)

vbool :: Badness -> VBool -> Property
vbool bad x = badness bad (-howTrue x) (isTrue x)

prop_max_speed :: Property
prop_max_speed =
  withBadness $ \bad ->
  withTestCase $ \_ test ->
    vbool bad $
    foldl' (&&+) true
      [ speed output <=% 140 ||+ rpm output <=% 5000 | (_, output) <- test ]

prop_two_one_two :: Property
prop_two_one_two =
  withBadness $ \bad ->
  withTestCase $ \delta test ->
    let
      size = truncate (2.5 / delta)
      windows = map (take size) (tails test)
    in
      and [ not ([2,1,2] `isSubsequenceOf` map (gear . snd) window) | window <- windows ]
