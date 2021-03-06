{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IFC where

import Data.List
import Data.Maybe
import Test.QuickCheck

import VBool
import Neighbour
import Shape

data Instr = Push LInt | Pop | Load | Store | Add | Noop | Halt
  deriving (Ord, Eq, Show)

instance HasShape Instr where
  shapeOf i = case i of
    Push li -> Nested 0 [shapeOf li]
    Pop     -> Nested 0 [Nary 0]
    Load    -> Nested 0 [Nary 0]
    Store   -> Nested 0 [Nary 0]
    Add     -> Nested 0 [Nary 0]
    Noop    -> Nested 0 [Nary 0]
    Halt    -> Nested 0 [Nary 0]

  fromRn (Nested 0 [lis]) xs = Push (fromRn lis xs)
  fromRn (Nested _ _) [x]  = case round x of
    0 -> Pop
    1 -> Load
    2 -> Store
    3 -> Add
    4 -> Noop
    5 -> Halt

  measure (Push li) = measure li
  measure i         = [case i of
    Pop  -> 0
    Load -> 1
    Store -> 2
    Add -> 3
    Noop -> 4
    Halt -> 5]

instance Arbitrary Instr where
  arbitrary = oneof [ Push <$> arbitrary
                    , return Pop
                    , return Load
                    , return Store
                    , return Add
                    , return Noop
                    , return Halt
                    ]

newtype Program = Program [Instr] deriving (Ord, Eq, Show, HasShape)

instance HasNeighbours Program where
  neighbours (Program p) = [ Program (p ++ [i]) |
                              i <- (map (\k -> Push (k, L))) [0..5] ++
                                   (map (\k -> Push (k, H))) [0..5] ++
                                   [Pop, Load, Store, Add] ]

instance Arbitrary Program where
  arbitrary = Program <$> arbitrary

data Label = L | H deriving (Ord, Eq, Show)

instance HasShape Label where
  shapeOf _ = Nary 1

  fromRn _ [x] = if x <= 0 then L else H

  measure L = [-1]
  measure H = [1]

instance Arbitrary Label where
  arbitrary = oneof [return L, return H]

instance HasNeighbours Label where
  neighbours l = [swap l]

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

instance Arbitrary MachineState where
  arbitrary = MachineState <$>
                 arbitrary <*>
                 arbitrary <*>
                 arbitrary <*>
                 arbitrary

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
                                if x >= 0 then
                                  Ok $ st { pc = pc st + 1
                                          , stack =
                                              (extendTo x (memory st)) !! x : xs
                                          }
                                else
                                  Failed
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
indist_instr i j = i ==% j ||+
  (case (i, j) of (Push v0, Push v1) -> indist_value v0 v1; _ -> false)

(~=) :: MachineState -> MachineState -> VBool
m0 ~= m1 = length (memory m0) ==% length (memory m1) &&+
           length (program m0) ==% length (program m1) &&+
           andP (zipWith indist_instr (program m0) (program m1)) &&+
           andP (zipWith indist_value (memory m0) (memory m1))

prop_EENI :: (MachineState -> Possibility)
          -> (Program, [LInt], [LInt])
          -> VBool
prop_EENI step (p0@(Program p), m0, m1) =
  let mak0 = mkMach m0
      mak1 = mkMach m1
  in (mak0 ~= mak1 &&+ halts step p0) %==>
  (maybe false id ((~=) <$> run step mak0 <*> run step mak1))
  where
    mkMach m = MachineState { pc = 0
                            , stack = []
                            , memory = m
                            , program = p ++ [Halt]
                            }

prop_EENI' :: (MachineState -> Possibility)
           -> [LInt]
           -> [LInt]
           -> Property
prop_EENI' step m0 m1 =
  forAll (hillClimb 10 (const False) (Program []) (halts step)) $
    \ (Program p)->
      let mak0 = mkMach m0
          mak1 = mkMach m1
          mkMach m = MachineState { pc = 0
                                  , stack = []
                                  , memory = m
                                  , program = p ++ [Halt]
                                  }
      in (toBool $ mak0 ~= mak1) ==>
         (maybe False toBool ((~=) <$> run step mak0 <*> run step mak1))
