-- The interface to the heater model.

{-# LANGUAGE ForeignFunctionInterface #-}
module HeaterModel(Input(..), Output(..), stepSize, runModel) where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import System.IO.Unsafe

#include "Heater.h"

data Input =
  Input {
    goal :: (#type real_T) }
  deriving (Eq, Show)

data Output =
  Output {
    time        :: Integer,
    temperature :: (#type real_T),
    boiler      :: (#type real_T),
    level       :: (#type real_T) }
  deriving Show

data CModel
data CInput
data COutput

foreign import ccall unsafe "Heater_initialize" c_initialise :: IO ()
foreign import ccall unsafe "Heater_terminate" c_terminate :: IO ()
foreign import ccall unsafe "Heater_step" c_step :: IO ()
foreign import ccall unsafe "&Heater_M" c_model :: Ptr (Ptr CModel)
foreign import ccall unsafe "&Heater_U" c_input :: Ptr CInput
foreign import ccall unsafe "&Heater_Y" c_output :: Ptr COutput

peekModel :: Storable a => (Ptr CModel -> Ptr a) -> IO a
peekModel f = peek c_model >>= peek . f

pokeInput :: Storable a => (Ptr CInput -> Ptr a) -> a -> IO ()
pokeInput f x = poke (f c_input) x

peekOutput :: Storable a => (Ptr COutput -> Ptr a) -> IO a
peekOutput f = peek (f c_output)

clockTick0 :: IO (#type uint32_T)
clockTick0 = peekModel (#ptr RT_MODEL_Heater_T, Timing.clockTick0)

stepSize0 :: IO (#type time_T)
stepSize0 = peekModel (#ptr RT_MODEL_Heater_T, Timing.stepSize0)

putInput :: Input -> IO ()
putInput input = do
  pokeInput (#ptr ExtU_Heater_T, Reference) (goal input)

getOutput :: IO Output
getOutput = do
  time <- fromIntegral <$> clockTick0
  temperature <- peekOutput (#ptr ExtY_Heater_T, t)
  boiler <- peekOutput (#ptr ExtY_Heater_T, h)
  level <- peekOutput (#ptr ExtY_Heater_T, l)
  return Output{time = time, temperature = temperature, boiler = boiler, level = level}

runModel :: [Input] -> [Output]
runModel xs =
  unsafePerformIO $ c_initialise *> loop xs <* c_terminate
  where
    loop [] = return []
    loop (inp:inps) = do
      putInput inp
      c_step
      output <- getOutput
      fmap (output:) $ loop inps

stepSize :: IO Double
stepSize =
  c_initialise *> stepSize0 <* c_terminate
