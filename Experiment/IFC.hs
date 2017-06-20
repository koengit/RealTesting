module IFC where

import Data.List
import Test.QuickCheck

import VBool
import Neighbour
import Shape

data Instr = Push LInt | Pop | Load | Store | Add | Noop | Halt
  deriving (Ord, Eq, Show)

newtype Program = Program [Instr] deriving (Ord, Eq, Show)

instance HasNeighbours Program where
  neighbours (Program p) = [ Program (p ++ [i]) |
                              i <- (map (\k -> Push (k, L))) [0..5] ++
                                   (map (\k -> Push (k, H))) [0..5] ++
                                   [Pop, Load, Store, Add] ]

data Label = L | H deriving (Ord, Eq, Show)

instance HasShape Label where
  shapeOf _ = Nary 1

  fromRn _ [x] = if x <= 0 then L else H

  measure L = [-1]
  measure H = [1]

instance Arbitrary Label where
  arbitrary = oneof [return L, return H]

swap :: Label -> Label
swap L = H
swap H = L

lub :: Label -> Label -> Label
lub l l' 
  | l == l'   = l
  | otherwise = H

type LInt  = (Int, Label)

data MachineState = MachineState {
                      pc :: Int
                    , stack :: [LInt]
                    , memory :: [LInt]
                    , program :: [Instr] }
                    deriving (Ord, Eq, Show)

machine :: Program -> MachineState
machine (Program instrs) =
  MachineState { pc      = 0
               , stack   = []
               , memory  = []
               , program = instrs ++ [Halt] }

setAt :: [a] -> LInt -> a -> [a]
setAt xs (i, l) d = take i xs ++ [d] ++ (drop (i + 1) xs)

extendTo :: Int -> [LInt] -> [LInt]
extendTo x m = take (max (x + 1) (length m)) $ m ++ repeat (0, L)

data Possibility = Ok MachineState | Halted | Failed deriving (Ord, Eq, Show)

-- | Section 2.4 of IFC testing paper by John etc.
step_naive :: MachineState -> Possibility
step_naive st
  | pc st >= length (program st) = Failed
  | otherwise = case program st !! pc st of
                  Noop   -> Ok $ st { pc = pc st + 1 }

                  Push x -> Ok $ st { pc    = pc st + 1
                                    , stack = x : stack st }

                  Pop    -> case stack st of
                              []     -> Failed
                              (x:xs) -> Ok $ st { pc    = pc st + 1
                                                , stack = xs }
                  
                  Load  -> case stack st of
                              []      -> Failed 
                              ((x, l):xs)  ->
                                  Ok $ st { pc = pc st + 1
                                          , stack =
                                              (extendTo x (memory st)) !! x : xs
                                          }
                  Store -> case stack st of
                            (x:y:xs) -> Ok $ st { pc     = pc st + 1
                                                , stack  = xs
                                                , memory = setAt
                                                    (extendTo (fst x)
                                                              (memory st))
                                                    x y}
                            _        -> Failed

                  Add   -> case stack st of
                            ((x, _):(y, _):xs) ->
                                       Ok $ st { pc = pc st + 1
                                               , stack = ((x + y), L):xs }
                            _ -> Failed

                  Halt -> Halted

run :: (MachineState -> Possibility) -> MachineState -> Maybe MachineState
run step ms = case step ms of
  Halted -> Just ms
  Failed -> Nothing
  Ok ms' -> run step ms'

--------------------

halts :: (MachineState -> Possibility) -> Program -> VBool
halts step p = go (machine p)
  where
    go m = case step m of
      Halted -> true
      Failed -> false 
      Ok ms' -> true &&+ go ms'

indist_value :: LInt -> LInt -> VBool
indist_value (i, l) (j, l') =
  (l ==% H &&+ l' ==% H) ||+ (i ==% j &&+ l ==% L &&+ l' ==% L)

indist_instr :: Instr -> Instr -> VBool
indist_instr _ _ = undefined
