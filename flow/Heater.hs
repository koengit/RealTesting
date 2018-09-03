module Heater where

import System
import qualified Data.Map as Map

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
system :: Control -> System
system control =
  System {
    inputVars = Map.singleton goalTemp RealType,
    process = parP [plant, controller control] }

-- the plant
-- input: pump, output: roomTemp

plant :: Process
plant = parP [room, heater]
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
  withIntegral err $ \int ->
  let
    pump' = Const k_p * err
          + Const k_i * int
  in
    continuous pump 0 ((pump' `minn` 1) `maxx` 0)
  where
    err   = Var goalTemp - Var roomTemp
          -- + val k_d * deriv err

-- XXX deal with derivatives, resets

-- controlleR :: Control -> S Temp -> S Temp -> S Level
-- controlleR (k_p,k_i,k_d) goalTemp roomTemp =
--   (pump' >=? 0) ? ((1 >=? pump') ? (pump', 1), 0)
--  where
--   err   = goalTemp - roomTemp
--   pump' = val k_p * err
--         + val k_i * integ (err `in1t` 0 `reset` (0 `when` changeGoalTemp))
--         + val k_d * deriv err

--   changeGoalTemp = abs (goalTemp - pre goalTemp) >? 1

cgood :: Control
--cgood = (3.997591176733649e-3,8.194771741046325e-5,5.618398605936785e-3)
--cgood = (5.0e-3,1.1446889636416996e-4,5.0e-3)
cgood = (1.2e-2,1.1446889636416996e-4,5.0e-3)
