module Falsify ( module VBool
               , module Shape
               , module Optimize
               , falsifyWithTrace
               , satisfyWithTrace
               , falsify
               , satisfy
               , satisfyPrecondition ) where
import VBool
import Shape
import Optimize

import Test.QuickCheck
import Data.Maybe

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
      (v, tr)      = patternSearch
                      it
                      initialPoint
                      delta
                      eps
                      (unVBool . prop . fromRn shapeOfa)
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
