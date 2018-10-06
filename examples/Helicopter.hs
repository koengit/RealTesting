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
    go env = env:go (update env)
    update env =
      fst (runIdentity (execStep delta (Map.insert input (DoubleValue (controlled env)) env) (step p)))

    controlled :: Env -> Double
    controlled env =
      theInput + ((ref future - free future) / response future)
      where
        DoubleValue theInput = Map.findWithDefault (DoubleValue 0) input env
        DoubleValue theOutput = Map.findWithDefault (DoubleValue 0) output env

        setpoint = 1
        err = setpoint - theOutput
        coeff = 0.3
        future = 1
        ref i = setpoint - exp (-fromIntegral i * coeff) * err
        free i = s
          where
            Just (DoubleValue s) = Map.lookup output env'
            env' = foldn i (fst . runIdentity . flip (execStep 1) (step p) . Map.insert input (DoubleValue theInput)) env

        response i =
          case Map.lookup output env of
            Just (DoubleValue x) -> x
          where
            env = last (fst (runIdentity (simulate 1 (replicate i (Map.singleton input (DoubleValue 1))) p)))
