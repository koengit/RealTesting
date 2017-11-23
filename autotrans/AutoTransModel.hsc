-- The interface to the automatic transmission model.

{-# LANGUAGE ForeignFunctionInterface #-}
module AutoTransModel(Input(..), Output(..), stepSize, runModel) where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import System.IO.Unsafe

#include "Autotrans_shift.h"

data Input =
  Input {
    throttle :: (#type real_T),
    brake    :: (#type real_T) }
  deriving Show

data Output =
  Output {
    time     :: Integer,
    speed    :: (#type real_T),
    rpm      :: (#type real_T),
    gear     :: (#type real_T) }
  deriving Show

data CModel
data CInput
data COutput

foreign import ccall unsafe "Autotrans_shift_initialize" c_initialise :: IO ()
foreign import ccall unsafe "Autotrans_shift_terminate" c_terminate :: IO ()
foreign import ccall unsafe "Autotrans_shift_step" c_step :: IO ()
foreign import ccall unsafe "&Autotrans_shift_M" c_model :: Ptr (Ptr CModel)
foreign import ccall unsafe "&Autotrans_shift_U" c_input :: Ptr CInput
foreign import ccall unsafe "&Autotrans_shift_Y" c_output :: Ptr COutput

peekModel :: Storable a => (Ptr CModel -> Ptr a) -> IO a
peekModel f = peek c_model >>= peek . f

pokeInput :: Storable a => (Ptr CInput -> Ptr a) -> a -> IO ()
pokeInput f x = poke (f c_input) x

peekOutput :: Storable a => (Ptr COutput -> Ptr a) -> IO a
peekOutput f = peek (f c_output)

clockTick0 :: IO (#type uint32_T)
clockTick0 = peekModel (#ptr RT_MODEL_Autotrans_shift_T, Timing.clockTick0)

stepSize0 :: IO (#type time_T)
stepSize0 = peekModel (#ptr RT_MODEL_Autotrans_shift_T, Timing.stepSize0)

putInput :: Input -> IO ()
putInput input = do
  pokeInput (#ptr ExtU_Autotrans_shift_T, throttle) (throttle input)
  pokeInput (#ptr ExtU_Autotrans_shift_T, brake) (brake input)

getOutput :: IO Output
getOutput = do
  time <- fromIntegral <$> clockTick0
  speed <- peekOutput (#ptr ExtY_Autotrans_shift_T, speed)
  rpm <- peekOutput (#ptr ExtY_Autotrans_shift_T, RPM)
  gear <- peekOutput (#ptr ExtY_Autotrans_shift_T, gear)
  return Output{time = time, speed = speed, rpm = rpm, gear = gear}

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
