{-# LANGUAGE StandaloneDeriving, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module AutoTrans where

import AutoTransModel
import Plot
import Test.QuickCheck
import GHC.Generics
import VBool
import Badness
import Data.List
import Data.Maybe
import System.Random
import Shape
import Optimize

choose' :: (Ord a, Random a, Fractional a) => (a, a) -> Gen a
choose' (x, y) =
  clamp <$> choose (x - delta, y + delta)
  where
    delta = (y - x) / 10
    clamp z
      | z < x = x
      | z > y = y
      | otherwise = z

inputOk :: Input -> VBool
inputOk input = ok (throttle input) &&+ ok (brake input)
  where
    ok x = 0 <=% x &&+ x <=% 100

deriving instance Generic Input
instance Arbitrary Input where
  arbitrary = do
    throttle <- choose' (0, 100)
    let brake = 0
    return Input{throttle = throttle, brake = brake}
  shrink = filter (isTrue . inputOk) . genericShrink

instance HasShape Input where
  shapeOf _ = shapeOf (undefined :: (Double, Double))
  fromRn s xs = Input x y where (x, y) = fromRn s xs
  measure (Input x y) = measure (x, y)

data Line = Line { start :: Input, end :: Input } deriving (Show, Generic)

lineOk :: Line -> VBool
lineOk (Line x y) = inputOk x &&+ inputOk y

instance Arbitrary Line where
  arbitrary =
    oneof [
      Line <$> arbitrary <*> arbitrary,
      (\x -> Line x x) <$> arbitrary]
  shrink (Line x y)
    | x == y = (\x -> Line x x) <$> shrink x
    | otherwise = Line x x:Line y y:Line (bin avg x y) (bin avg x y):genericShrink (Line x y)
    where
      avg x y = (x+y)/2

instance HasShape Line where
  shapeOf _ = shapeOf (undefined :: (Input, Input))
  fromRn s xs = Line x y where (x, y) = fromRn s xs
  measure (Line x y) = measure (x, y)

bin :: (Double -> Double -> Double) -> Input -> Input -> Input
bin op x y =
  Input {
    throttle = throttle x `op` throttle y,
    brake = brake x `op` brake y }       

diff :: Line -> Input
diff line = bin (-) (end line) (start line)

newtype Inputs = Inputs [(Double, Line)] deriving (Show, HasShape)

inputsOk :: Inputs -> VBool
inputsOk (Inputs xs) = conj (map lineOk (map snd xs))

instance Arbitrary Inputs where
  arbitrary = do
    duration <- sized $ \n -> choose (0, fromIntegral (10*n))
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

withTestCase :: Testable prop => (Double -> [Input] -> [Output] -> prop) -> Property
withTestCase prop = property $ \inps -> ioProperty $ do
  delta <- stepSize
  let inputs = sampleInputs delta inps
      outputs = runModel inputs
      graph xs = (map (* delta) [0..], xs)
  return $
    whenFail (plotIt delta inputs outputs) $
      prop delta inputs outputs

plotIt :: Double -> [Input] -> [Output] -> IO ()
plotIt delta inputs outputs =
  plot "failed" (ceiling (fromIntegral (length inputs) / delta))
      [("throttle", graph (map throttle inputs)),
       ("brake", graph (map brake inputs)),
       ("speed", graph (map speed outputs)),
       ("rpm", graph (map rpm outputs)),
       ("gear", graph (map gear outputs))]

prop_max_speed :: Property
prop_max_speed =
  withBadness $ withTestCase $ \_ _ outputs ->
    prop_max_speed_vbool outputs

prop_max_speed_vbool :: [Output] -> VBool
prop_max_speed_vbool outputs =
  --foldl' (&&%) true
  conj
  [ (speed output <=% 120 ||+ rpm output <=% 4500) | output <- outputs ] # (1 / fromIntegral (length outputs))

prop_two_one_two :: Property
prop_two_one_two =
  withBadness $ withTestCase $ \delta _ outputs ->
    prop_two_one_two_vbool delta outputs

prop_two_one_two_vbool :: Double -> [Output] -> VBool
prop_two_one_two_vbool delta outputs =
  let
    size = truncate (2.5 / delta)
    p (2:1:xs) =
      case elemIndex 2 (take size xs) of
        Nothing -> true
        Just i  -> bad (size - i)
    p _ = true
  in
    conj (map p (tails (map gear outputs)))

prop_engine_speed :: Property
prop_engine_speed =
  withBadness $ withTestCase $ \delta _ outputs ->
    prop_engine_speed_vbool delta outputs

prop_engine_speed_vbool :: Double -> [Output] -> VBool
prop_engine_speed_vbool delta outputs =
  conj [ rpm output <=% 5500 | output <- outputs ] ==>%
  conj [ speed output <=% 120 | output <- take (truncate (30 / delta)) outputs ]

prop_speed :: Property
prop_speed =
  withBadness $ withTestCase $ \delta _ outputs ->
    prop_speed_vbool delta outputs

prop_speed_vbool :: Double -> [Output] -> VBool
prop_speed_vbool delta outputs =
  let
    t = 30
    w = 4000
    v = 120

    prop [] = false
    prop (x:xs) =
      speed x >=% v &&+ conj [ rpm x <=% w | x <- xs ]
  in
    length outputs >=% truncate ((t*1.5) / delta) ==>%
    nt (conj [ rpm output <% w | output <- outputs ] &&+
        disj [ speed output >% v | output <- take (truncate (t / delta)) outputs ])

optimiseVBool :: (Double -> [Input] -> [Output] -> VBool) -> IO ()
optimiseVBool f = do
  mapM_ pr results
  plotRes (last results)
  where
    results = take 1000 (minimize (measure inps) (measure inps) g)
    delta = 0.01
    inps = Inputs (replicate 10 (10, Line (Input 50 50) (Input 50 50)))
    s = shapeOf inps
    g x =
      inputsOk inps ==>% f delta inputs (runModel inputs)
      where
        inps = fromRn s x
        inputs = sampleInputs delta inps

    pr (_, x, y, _) =
      putStrLn (show (fromRn s x :: Inputs) ++ " -> " ++ show y)

    plotRes (_, x, _, _) =
      plotIt delta inputs (runModel inputs)
      where
        inputs = sampleInputs delta (fromRn s x)
