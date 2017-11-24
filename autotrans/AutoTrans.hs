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

data Line = Line { start :: Input, end :: Input } deriving (Show, Generic)

instance Arbitrary Line where
  arbitrary =
    oneof [
      Line <$> arbitrary <*> arbitrary,
      (\x -> Line x x) <$> arbitrary]
  shrink = genericShrink

bin :: (Double -> Double -> Double) -> Input -> Input -> Input
bin op x y =
  Input {
    throttle = throttle x `op` throttle y,
    brake = brake x `op` brake y }       

diff :: Line -> Input
diff line = bin (-) (end line) (start line)

newtype Inputs = Inputs [(Double, Line)] deriving Show

instance Arbitrary Inputs where
  arbitrary = do
    duration <- sized $ \n -> choose (0, fromIntegral (10*n))
    cut duration <$> Inputs <$>
      infiniteListOf (do
        duration <- choose (0, 20)
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
  return $
    whenFail (plot "failed" (ceiling (fromIntegral (length inputs) / delta))
      [("throttle", graph (map throttle inputs)),
       ("brake", graph (map brake inputs)),
       ("speed", graph (map speed outputs)),
       ("rpm", graph (map rpm outputs)),
       ("gear", graph (map gear outputs))]) $
      prop delta (zip inputs outputs)

prop_max_speed :: Property
prop_max_speed =
  withBadness $ withTestCase $ \_ test ->
    conj [ speed output <=% 140 ||+ rpm output <=% 4500 | (_, output) <- test ] # (1000000000 / (fromIntegral (length test)))

prop_two_one_two :: Property
prop_two_one_two =
  withBadness $ withTestCase $ \delta test ->
    let
      size = truncate (2.5 / delta)
      p (2:1:xs) =
        case elemIndex 2 (take size xs) of
          Nothing -> true
          Just i  -> false #+ fromIntegral (size - i)
      p _ = true
    in
      conj (map p (tails (map (gear . snd) test))) # (1000000000 / (fromIntegral (length test)))
