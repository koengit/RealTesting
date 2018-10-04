-- A helicopter using model predictive control.
module Helicopter where

import Process
import Process.Simplify
import Process.Language
import Data.Either
import Data.Generics.Uniplate.Data
import Utils
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as Map
import Data.Functor.Identity

angle, speed, s :: Var
angle = Global "angle"
speed = Global "speed"
s = Global "s"

helicopter =
  differentialEquation speed 0 $
    transferFunction speed angle s helicopterTransfer

helicopterTransfer =
  (9.8 * (var s^2 - 0.5*var s + 6.3) /
    ((var s + 0.6565) * (var s^2 - 0.2366 * var s + 0.1493)))

testCase =
  [12,4,5,-2,3,1,5,2.5,6,4.5,8,7.5,10.25,9.75,12.5,12,14,13.5,15.25,14.75] ++
  replicate 50 16

test = fst (runIdentity $ simulate 0.01 (concat [replicate 60 (Map.singleton angle (DoubleValue (x/10000))) | x <- testCase]) (lower stdPrims helicopter))
plotIt = plot "helicopter" 0.01 test helicopter

