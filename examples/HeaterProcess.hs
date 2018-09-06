-- module Heater where

import Process
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

-- the system
system :: Control -> Process
system control = plant & controller control

systeM :: Control -> Process
systeM control = plant & controlleR control

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
      weigh [ (1-Var pump,  Var heaterTemp)
            , (Var pump,    boilerTemp)
            , (heaterCoeff, Var roomTemp)
            ]

  -- the room temperature is influenced by the heater temperature and the outside
  -- temperature
  room =
    continuous roomTemp startTemp $
      weigh [ (1,            Var roomTemp)
            , (heaterCoeff,  Var heaterTemp)
            , (outsideCoeff, outsideTemp)
            ]

-- plant

type Control = (Double, Double, Double)

controller :: Control -> Process
controller (k_p,k_i,k_d) =
  continuous pump 0 $ clamp 0 1 $
      Const k_p * err
    + Const k_i * integral err
    + Const k_d * Deriv err
  where
    err = Var goalTemp - Var roomTemp

controlleR :: Control -> Process
controlleR (k_p,k_i,k_d) =
  continuous pump 0 $ clamp 0 1 $
      Const k_p * err
    + Const k_i * IntegralReset err changeGoalTemp
    + Const k_d * Deriv err
 where
  err = Var goalTemp - Var roomTemp

  changeGoalTemp = abs (Var goalTemp - Old 0 (Var goalTemp)) /=? 1

cgood :: Control
--cgood = (3.997591176733649e-3,8.194771741046325e-5,5.618398605936785e-3)
--cgood = (5.0e-3,1.1446889636416996e-4,5.0e-3)
cgood = (1.2e-2,1.1446889636416996e-4,5.0e-3)

test = last $ fst $ runIdentity $ simulate 1 (replicate 300 (Map.singleton goalTemp (DoubleValue 25))) (system cgood)
tesT = last $ fst $ runIdentity $ simulate 1 (replicate 300 (Map.singleton goalTemp (DoubleValue 25))) (systeM cgood)

test' = mapVal (last . fst) $ simulate 1 (replicate 300 (Map.singleton goalTemp (DoubleValue 25))) (system cgood)
tesT' = mapVal (last . fst) $ simulate 1 (replicate 300 (Map.singleton goalTemp (DoubleValue 25))) (systeM cgood)

main =
  sequence_ [putStrLn (show v ++ ": " ++ show k) | (k, v) <- xs]
  where
    Val xs = test'
