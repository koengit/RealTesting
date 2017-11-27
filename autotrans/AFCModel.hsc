-- The interface to the abstract fuel control model.

{-# LANGUAGE ForeignFunctionInterface #-}
module AFCModel(Input(..), Output(..), stepSize, runModel) where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import System.IO.Unsafe

#include "AbstractFuelControl_M1.h"

data Input =
  Input {
    throttleAngle :: (#type real_T),
    engineSpeed   :: (#type real_T) }
  deriving (Eq, Show)

data Output =
  Output {
    time     :: Integer,
    airFuelRatioRef :: (#type real_T),
    airFuelRatio    :: (#type real_T),
    mode            :: (#type real_T) }
  deriving Show

data CModel
data CInput
data COutput

foreign import ccall unsafe "AbstractFuelControl_M1_initialize" c_initialise :: IO ()
foreign import ccall unsafe "AbstractFuelControl_M1_terminate" c_terminate :: IO ()
foreign import ccall unsafe "AbstractFuelControl_M1_step" c_step :: IO ()
foreign import ccall unsafe "&AbstractFuelControl_M1_M" c_model :: Ptr (Ptr CModel)
foreign import ccall unsafe "&AbstractFuelControl_M1_U" c_input :: Ptr CInput
foreign import ccall unsafe "&AbstractFuelControl_M1_Y" c_output :: Ptr COutput

peekModel :: Storable a => (Ptr CModel -> Ptr a) -> IO a
peekModel f = peek c_model >>= peek . f

pokeInput :: Storable a => (Ptr CInput -> Ptr a) -> a -> IO ()
pokeInput f x = poke (f c_input) x

peekOutput :: Storable a => (Ptr COutput -> Ptr a) -> IO a
peekOutput f = peek (f c_output)

clockTick0 :: IO (#type uint32_T)
clockTick0 = peekModel (#ptr RT_MODEL_AbstractFuelControl__T, Timing.clockTick0)

stepSize0 :: IO (#type time_T)
stepSize0 = peekModel (#ptr RT_MODEL_AbstractFuelControl__T, Timing.stepSize0)

putInput :: Input -> IO ()
putInput input = do
  pokeInput (#ptr ExtU_AbstractFuelControl_M1_T, PedalAngle) (throttleAngle input)
  pokeInput (#ptr ExtU_AbstractFuelControl_M1_T, EngineSpeed) (engineSpeed input)

getOutput :: IO Output
getOutput = do
  time <- fromIntegral <$> clockTick0
  airFuelRatioRef <- peekOutput (#ptr ExtY_AbstractFuelControl_M1_T, AFref)
  airFuelRatio <- peekOutput (#ptr ExtY_AbstractFuelControl_M1_T, AF)
  mode <- peekOutput (#ptr ExtY_AbstractFuelControl_M1_T, controller_mode)
  return Output{time = time, airFuelRatioRef = airFuelRatioRef, airFuelRatio = airFuelRatio, mode = mode}

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
