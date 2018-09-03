{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module System where

import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Generics.Geniplate -- cabal install geniplate-mirror

-- TO DO:
-- * work out how to deal with derivative in pid controller
-- * work out how to do resetting of integral
-- * add a nicer way of generating temporary variables (e.g. for storing integral)

-- Input signals have a type
data Type = RealType | IntegerType deriving (Eq, Show)

newtype Var = V String deriving (Eq, Ord, Show)

data System =
  System {
    inputVars :: Map Var Type, -- input signals
    process :: Process         -- the code the system executes
  } deriving Show

-- A process consists of an initialisation step
-- followed by a loop step that runs repeatedly
data Process =
  Process {
    start :: Step, -- initialisation
    step :: Step   -- loop step
  } deriving Show

data Step =
   If Expr Step Step     -- if-then-else
 | Assume Expr Step      -- check precondition
 | Assert Expr Step      -- check postcondition
 | Update (Map Var Expr) -- update variables
 deriving Show

data Expr =
   Var Var -- variable
 | Delta   -- delta-t
   -- Arithmetic
 | Const Double
 | Plus Expr Expr
 | Times Expr Expr
 | Power Expr Expr
 | Negate Expr
 | Min Expr Expr -- minimum
 | Sin Expr
   -- Booleans
 | Not Expr
 | And Expr Expr
 | Tru
 | Positive Expr -- e >= 0
 | Zero Expr     -- e == 0
 deriving (Eq, Show)

----------------------------------------------------------------------
-- free variables of expressions

instanceUniverseBi [t| (Expr, Var)|]
instanceUniverseBi [t| (Step, Var)|]

class Vars a where
  vars :: a -> Set Var

instance Vars Expr where
  vars = Set.fromList . universeBi

instance Vars Step where
  vars = Set.fromList . universeBi

----------------------------------------------------------------------
-- simple combinators for building processes

instance Num Expr where
  fromInteger = Const . fromInteger
  x + y = Plus x y
  x * y = Times x y
  negate x = Negate x
  abs = error "Expr: abs not supported"
  signum = error "Expr: signum not supported"

instance Fractional Expr where
  fromRational = Const . fromRational
  recip x = Power x (-1)

-- Parallel composition of processes
parP :: [Process] -> Process
parP ps =
  Process {
    start = parS (map start ps),
    step = parS (map step ps) }

skip :: Step
skip = Update Map.empty

-- Parallel composition of steps
parS :: [Step] -> Step
parS = foldr par skip
  where
    par (If e s1 s2) s3 =
      If e (par s1 s3) (par s2 s3)
    par (Assume e s1) s2 =
      Assume e (par s1 s2)
    par (Assert e s1) s2 =
      Assert e (par s1 s2)
    par (Update m1) (Update m2) =
      update (Map.toList m1 ++ Map.toList m2)
    par s1@Update{} s2 = par s2 s1

-- Build an update step
update :: [(Var, Expr)] -> Step
update = Update . Map.fromListWith f
  where
    f x y
      | x == y = x
      | otherwise = error "parS: incoherent state update"

-- Define a variable whose value changes with every step
continuous :: Var -> Expr -> Expr -> Process
continuous x start step =
  Process {
    start = update [(x, start)],
    step = update [(x, step)] }

-- Define a variable which is the integral of some expression
integral :: Var -> Expr -> Process
integral x step =
  continuous x 0 (Var x + Delta * step)

-- Minimum and maximum
minn, maxx :: Expr -> Expr -> Expr
minn = Min
maxx x y = negate (Min (negate x) (negate y))

----------------------------------------------------------------------
-- evaluation

type Env = Map Var Value
data Value = DoubleValue Double | BoolValue Bool deriving (Eq, Show)

double :: Value -> Double
double (DoubleValue x) = x
double _ = error "type error"

bool :: Value -> Bool
bool (BoolValue x) = x
bool _ = error "type error"

eval :: Double -> Env -> Expr -> Value
eval _ env (Var x) =
  Map.findWithDefault (error "variable not bound") x env
eval delta _ Delta =
  DoubleValue delta
eval _ _ (Const x) =
  DoubleValue x
eval delta env (Plus e1 e2) =
  DoubleValue (double (eval delta env e1) + double (eval delta env e2))
eval delta env (Times e1 e2) =
  DoubleValue (double (eval delta env e1) * double (eval delta env e2))
eval delta env (Power e1 e2) =
  DoubleValue (double (eval delta env e1) ** double (eval delta env e2))
eval delta env (Negate e) =
  DoubleValue (negate (double (eval delta env e)))
eval delta env (Min e1 e2) =
  DoubleValue (double (eval delta env e1) `min` double (eval delta env e2))
eval delta env (Sin e) =
  DoubleValue (sin (double (eval delta env e)))
eval delta env (Not e) =
  BoolValue (not (bool (eval delta env e)))
eval delta env (And e1 e2) =
  BoolValue (bool (eval delta env e1) && bool (eval delta env e2))
eval _ _ Tru =
  BoolValue True
eval delta env (Positive e) =
  BoolValue (double (eval delta env e) >= 0)
eval delta env (Zero e) =
  BoolValue (double (eval delta env e) == 0)

data Result = OK | PreconditionFailed Expr | PostconditionFailed Expr
  deriving Show

execStep :: Double -> Env -> Step -> (Env, Result)
execStep delta env (If e s1 s2) =
  if bool (eval delta env e)
  then execStep delta env s1
  else execStep delta env s2
execStep delta env (Assume e s) =
  if bool (eval delta env e)
  then execStep delta env s
  else (env, PreconditionFailed e)
execStep delta env (Assert e s) =
  if bool (eval delta env e)
  then execStep delta env s
  else (env, PostconditionFailed e)
execStep delta env (Update m) =
  -- N.B. Map.union is left-biased
  (Map.union (Map.map (eval delta env) m) env, OK)

execProcess ::
  Double -> [Env] -> Process -> ([Env], Result)
execProcess delta inputs process =
  loop Map.empty [] (Map.empty:inputs) (start process:repeat (step process))
  where
    loop _ history [] _ = (reverse history, OK)
    loop env history (inp:inps) (step:steps) =
      case execStep delta (Map.union inp env) step of
        (env, OK) -> loop env (env:history) inps steps
        (env, err) -> (reverse (env:history), err)
