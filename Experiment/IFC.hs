module IFC where

data Instr = Push LInt | Pop | Load | Store | Add | Noop | Halt
  deriving (Ord, Eq, Show)

data Label = L | H deriving (Ord, Eq, Show)

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

machine :: [Instr] -> MachineState
machine instrs = MachineState { pc      = 0
                              , stack   = []
                              , memory  = []
                              , program = instrs }

setAt :: [a] -> LInt -> a -> [a]
setAt xs (i, l) d = take i xs ++ [d] ++ (drop (i + 1) xs)

extendTo :: Int -> [LInt] -> [LInt]
extendTo x m = take (max x (length m)) $ m ++ repeat (0, L)

data Possibility = Ok MachineState | Halted | Failed deriving (Ord, Eq, Show)

-- | Section 2.4 of IFC testing paper by John etc.
step_naive :: MachineState -> Possibility
step_naive st
  | pc st > length (program st) = Failed
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
                                          , stack = (extendTo x (memory st))
                                                  !! x : xs }
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
