-- Describing the set of test inputs for a system.
module Process.Input where

import Process.Language(Var)
import Process.Eval
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import Data.List

type Types = Map Var (Time, Type)
type Duration = Double

data Time =
    -- Continuous time: changes on every timestep
    -- Must have type Real
    Continuous
    -- Discrete time: only changes a finite number of times
  | Discrete
    -- Parameter: has the same value throughout execution
  | Parameter
  deriving (Eq, Show)

data Type = Real (Double, Double) | Integer (Integer, Integer) | Bool
  deriving (Eq, Show)

type Signal = [(Duration, Piece)]
data Piece = Linear (Double, Double) | Constant Value deriving Eq
instance Show Piece where
  show (Linear (x, y)) = show x ++ " -- " ++ show y
  show (Constant x) = show x

data Input = Input Duration (Map Var Signal)
instance Show Input where
  show (Input dur signals) =
    intercalate "\n" $
      ["Input of duration " ++ show dur ++ "s:"] ++
      map showSignal (Map.toList signals)
    where
      showSignal (x, sig) =
        "  " ++ show x ++ ": " ++ intercalate ", " (map showPart sig)
      showPart (x, p) =
        show p ++ " for " ++ show x ++ "s"

cut :: Double -> Signal -> Signal
cut x _ | x <= 0 = []
cut x [] = []
cut x ((y, line):inps)
  | x <= y = [(x, line)]
  | otherwise = (y, line):cut (x-y) inps

recut :: Double -> Signal -> Signal
recut 0 _ = []
recut x sig = cut x (extend sig)
  where
    extend [] = error "can't extend empty test case"
    extend [(x, v)] = [(1/0, v)]
    extend (x:xs) = x:extend xs

sampleSignal :: Double -> Signal -> [Value]
sampleSignal delta xs =
  concat (zipWith interpolate times lines)
  where
    times = map truncate (map (/ delta) lengths)
    (lengths, lines) = unzip xs
    interpolate n line =
      map (interp1 n line) [0..n-1]
    interp1 _ (Constant x) _ = x
    interp1 n (Linear (start, end)) i =
      DoubleValue (start + x * (end - start))
      where
        x = fromIntegral i / fromIntegral n

sampleInput :: Double -> Input -> [Env]
sampleInput delta (Input dur xs) =
  foldr (zipWith Map.union) (replicate n Map.empty) .
  map toEnv .
  Map.toList .
  Map.map (sampleSignal delta) $ xs
  where
    n = truncate (dur / delta)
    toEnv (x, xs) = map (Map.singleton x) xs
