-- A helicopter using model predictive control.
module Helicopter where

import Process
import Process.Simplify
import Process.Language
import Process.Eval
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

test' = take 6000 (control 0.01 speed angle 1 (lower stdPrims helicopter))
plotIt' = plot "helicopter" 0.01 test' helicopter

control :: Double -> Var -> Var -> Double -> Process -> [Env]
control delta output input setpoint p = go env0
  where
    env0 = fst (runIdentity (execStep delta (Map.singleton input (DoubleValue 0)) (start p)))
    go env = envs ++ go (last envs)
      where
        inp = controlled env
        envs = take (ceiling (tick / delta)) (tail (iterate (exec delta inp) env))
    exec delta x env = fst (runIdentity (execStep delta (Map.insert input (DoubleValue x) env) (step p)))
    get var env = x
      where
        Just (DoubleValue x) = Map.lookup var env

    setpoint = 1
    tick = 0.6
    speed = 6
    horizon = 8

    unitResponse =
      get output $
        foldn horizon (exec tick 1) env0

    controlled :: Env -> Double
    controlled env =
      get input env + (reference - freeResponse) / unitResponse
      where
        err = setpoint - get output env
        reference = setpoint - exp (-fromIntegral horizon * tick / speed) * err
        freeResponse =
          get output $
            foldn horizon (exec tick (get input env)) env
