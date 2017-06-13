module Falsify ( module VBool
               , module Shape
               , falsifyWithTrace
               , satisfyWithTrace
               , falsify
               , satisfy
               , satisfyPrecondition ) where
import VBool
import Shape

import Data.List
import Data.Ord
import Control.Parallel
import Test.QuickCheck
import Data.Maybe

setAt :: [Double] -> Int -> Double -> [Double]
setAt xs i d = take i xs ++ [d] ++ (drop (i + 1) xs)

patternMinimize :: Int                  -- ^ Number of steps
                -> [Double]             -- ^ Initial point
                -> Double               -- ^ Initial delta
                -> Double               -- ^ Epsilon
                -> ([Double] -> Double) -- ^ Function to minimize
                -> [[Double]]           -- ^ Acc
                -> ([Double], [[Double]])
patternMinimize 0 p _ _ _  acc = (p, p:acc)
patternMinimize n p d eps f acc
  | d < eps   = (p, p:acc)
  | otherwise =
    let lows   = [ setAt p i (p !! i + d) | i <- [0 .. length p - 1] ]
        highs  = [ setAt p i (p !! i - d) | i <- [0 .. length p - 1] ]
        points = highs `par` lows `pseq` lows ++ highs
        minP   = minimumBy (comparing f) points
    in if f p <= f minP then
         patternMinimize (n - 1) p (d / 2) eps f (p:acc)
       else
         patternMinimize (n - 1) minP d eps f (p:acc)

falsifyWithTrace :: HasShape a
        => a            -- ^ Initial guess for a
        -> Double       -- ^ Delta
        -> Int          -- ^ Iterations
        -> (a -> VBool) -- ^ Property to falsify
        -> (Maybe a, [a])
falsifyWithTrace a delta it prop =
  let initialPoint = measure a
      shapeOfa     = shapeOf a
      eps          = 0.1
      (v, tr)      = patternMinimize
                      it 
                      initialPoint
                      delta
                      eps
                      (unVBool . prop . fromRn shapeOfa)
                      []
      result       = fromRn shapeOfa v
      rtrace       = (fromRn shapeOfa) <$> tr
  in if not . toBool $ prop result then
        (Just result, rtrace)
     else
        (Nothing, rtrace)

satisfyWithTrace :: HasShape a
                 => a            -- ^ Initial guess for a
                 -> Double       -- ^ Delta
                 -> Int          -- ^ Iterations
                 -> (a -> VBool) -- ^ Property to falsify
                 -> (Maybe a, [a])
satisfyWithTrace a delta it prop = falsifyWithTrace a delta it (nt . prop)

satisfy, falsify :: HasShape a
                 => a            -- ^ Initial guess for a
                 -> Double       -- ^ Delta
                 -> Int          -- ^ Iterations
                 -> (a -> VBool) -- ^ Property to falsify
                 -> Maybe a
satisfy a delta it prop = fst $ satisfyWithTrace a delta it prop
falsify a delta it prop = fst $ falsifyWithTrace a delta it prop

satisfyPrecondition :: (Arbitrary a, HasShape a, Show a)
                    => (a -> VBool)
                    -> (a -> Property)
                    -> Property
satisfyPrecondition precond prop = forAll arbitrary $ \xs ->
  let satisfied = satisfy xs 15.0 200 precond in
    collect satisfied $ isJust satisfied ==> prop (fromJust satisfied)
