-- module Heater where

import Process
import Process.QuickCheck
import qualified Data.Map as Map
import Data.Functor.Identity
import Val(Val(..), mapVal)

--------------------------------------------------------------------------------
-- heater + plant

-- computing the weighted average

weigh :: Fractional a => [(a,a)] -> a
weigh axs = sum [ a*x | (a,x) <- axs ] / sum [ a | (a,_) <- axs ]

-- variables
goalTemp, roomTemp, heaterTemp, pump :: Var
goalTemp = Global "goalTemp"
roomTemp = Global "roomTemp"
heaterTemp = Global "heaterTemp"
pump = Global "pump"

-- input types
types :: Types
types = Map.singleton goalTemp (Discrete, Real (15, 25))

-- the system
system :: Control -> Process
system control = plant & controller control & property

systeM :: Control -> Process
systeM control = plant & controlleR control & property

-- the plant
-- input: pump, output: roomTemp

plant :: Process
plant = room & heater
 where
  startTemp    = outsideTemp
  boilerTemp   = 90
  heaterCoeff  = 0.1
  outsideTemp  = -5
  outsideCoeff = 0.05

  -- the heater temperature is influenced by how much hot water is pumped into it
  -- and the room temperature
  heater =
    continuous heaterTemp startTemp $
      weigh [ (1-var pump,  var heaterTemp)
            , (var pump,    boilerTemp)
            , (heaterCoeff, var roomTemp)
            ]

  -- the room temperature is influenced by the heater temperature and the outside
  -- temperature
  room =
    continuous roomTemp startTemp $
      weigh [ (1,            var roomTemp)
            , (heaterCoeff,  var heaterTemp)
            , (outsideCoeff, outsideTemp)
            ]

-- plant

type Control = (Double, Double, Double)

controller :: Control -> Process
controller (k_p,k_i,k_d) =
  continuous pump 0 $ clamp 0 1 $
      double k_p * err
    + double k_i * integral err
    + double k_d * deriv err
  where
    err = var goalTemp - var roomTemp

controlleR :: Control -> Process
controlleR (k_p,k_i,k_d) =
  continuous pump 0 $ clamp 0 1 $
      double k_p * err
    + double k_i * integralReset err changeGoalTemp
    + double k_d * deriv err
 where
  err = var goalTemp - var roomTemp

  changeGoalTemp = abs (var goalTemp - old 0 (var goalTemp)) >=? 1

cgood :: Control
cgood = (3.997591176733649e-3,8.194771741046325e-5,5.618398605936785e-3)
-- cgood = (5.0e-3,1.1446889636416996e-4,5.0e-3)
-- cgood = (1.2e-2,1.1446889636416996e-4,5.0e-3)

property :: Process
property =
  name $ \stablefor ->
    let stablefor = integralReset 1 (var goalTemp /=? old 0 (var goalTemp)) in
    loop (assert "temperature not close enough" (stablefor <=? 50 ||| abs (var goalTemp - var roomTemp) <=? 1))

run :: Valued f => Process -> f ([Env], Result)
run p = simulate 1 test (lower stdPrims p)
  where
    test = replicate 10 (temp 25) ++ replicate 90 (temp 10)
    temp k = Map.singleton goalTemp (DoubleValue 25)

test = runIdentity $ run (system cgood)
tesT = runIdentity $ run (systeM cgood)

test', tesT' :: Val ([Env], Result)
test' = run (system cgood)
tesT' = run (systeM cgood)

main =
  sequence_ [putStrLn (show v ++ ": " ++ show k) | (k, v) <- xs]
  where
    Val xs = test'

prop_heater =
  checkAssertions 1 300 types (lower stdPrims (system cgood))

prop_heateR =
  checkAssertions 1 300 types (lower stdPrims (systeM cgood))
