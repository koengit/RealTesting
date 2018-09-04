{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Process where

import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Generics.Geniplate -- cabal install geniplate-mirror
import Data.Tuple(swap)
import Data.Maybe

data Var =
    Global String
  | Local Int
  deriving (Eq, Ord, Show)

-- A process consists of an initialisation step
-- followed by a loop step that runs repeatedly
data Process =
  Process {
    locals :: Int, -- number of bound variables
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
 | Bool Bool
 | Positive Expr -- e >= 0
 | Zero Expr     -- e == 0
 | Cond Expr Expr Expr -- would be nice to be able to lower this
   -- These operations can be eliminated by calling 'lower'
 | Old Expr
 | IntegralReset Expr Expr -- first argument: quantity to integrate; second argument: reset if true
 deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- free variables of expressions

instanceUniverseBi [t| (Expr, Var)|]
instanceUniverseBi [t| (Step, Var)|]
instanceUniverseBi [t| (Process, Var)|]
instanceUniverseBi [t| (Process, Expr)|]
instanceUniverseBi [t| (Step, Map Var Expr)|]
instanceTransformBi [t| (Expr, Expr)|]
instanceTransformBi [t| (Expr, Step)|]
instanceTransformBi [t| (Expr, Process)|]
instanceTransformBi [t| (Var, Expr)|]
instanceTransformBi [t| (Var, Step)|]
instanceTransformBi [t| (Var, Process)|]

class (UniverseBi a Var, TransformBi Var a) => Vars a where
  vars :: a -> Set Var
  vars = Set.fromList . universeBi

  rename :: Vars a => (Var -> Var) -> a -> a
  rename = transformBi

instance Vars Expr
instance Vars Step
instance Vars Process

subst :: (Var -> Expr) -> Expr -> Expr
subst sub = transformBi f
  where
    f (Var x) = sub x
    f x = x

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

-- Make a simple process
process :: Step -> Step -> Process
process start step =
  Process {
    locals = 0,
    start = start,
    step = step }

initially, repeatedly :: Step -> Process
initially start = process start skipS
repeatedly step = process skipS step

-- The skip process
skipP :: Process
skipP = process skipS skipS

skipS :: Step
skipS = Update Map.empty

-- Updating a variable
set :: Var -> Expr -> Step
set x e = Update (Map.singleton x e)

-- Parallel composition of processes
parP :: [Process] -> Process
parP = foldr (combine parS2 parS2) skipP

-- Helper function for combining two processes
combine :: (Step -> Step -> Step) -> (Step -> Step -> Step) -> Process -> Process -> Process
combine startf stepf p q =
  Process {
    locals = locals p + locals q,
    start = startf (start p') (start q),
    step = stepf (step p') (step q) }
  where
    p' = rename shift p
    shift (Local x) | x < locals p = Local (locals q+x)
    shift x = x

-- Parallel composition of steps
parS :: [Step] -> Step
parS = foldr parS2 skipS

parS2 :: Step -> Step -> Step
parS2 (If e s1 s2) s3 =
  If e (parS2 s1 s3) (parS2 s2 s3)
parS2 (Assume e s1) s2 =
  Assume e (parS2 s1 s2)
parS2 (Assert e s1) s2 =
  Assert e (parS2 s1 s2)
parS2 (Update m1) (Update m2) =
  Update (Map.unionWith f m1 m2)
  where
    f x y
      | x == y = x
      | otherwise = error "parS: incoherent state update"
parS2 s1@Update{} s2 = parS2 s2 s1

-- Local name generation
name :: (Var -> Process) -> Process
name f = p{locals = locals p + 1}
  where
    p = f xs
    xs = Local (locals p)

-- Define a variable whose value changes with every step
continuous :: Var -> Expr -> Expr -> Process
continuous x start step =
  process (set x start) (set x step)

-- Derivative and integral
derivative :: Expr -> Expr
derivative e = (e - Old e) / Old Delta

integral :: Expr -> Expr
integral e = IntegralReset e (Bool False)

-- Minimum and maximum
minn, maxx :: Expr -> Expr -> Expr
minn = Min
maxx x y = negate (Min (negate x) (negate y))

-- Clamp a value to a range
clamp :: Expr -> Expr -> Expr -> Expr
clamp lo hi x =
  maxx lo (minn hi x)

-- Choose between two processes. Initial state executes both
switch :: Expr -> Process -> Process -> Process
switch cond p1 p2 =
  combine parS2 (If cond) p1 p2

----------------------------------------------------------------------
-- lowering operations

lower :: Process -> Process
lower p =
  case [(e, f) | e <- universeBi p, f <- maybeToList (lowerExpr e)] of
    [] -> p
    ((e, f):_) ->
      lower $
        name $ \x ->
          let
            (q, e') = f x
          in
            parP [replace e e' p, q]
  where
    lowerExpr :: Expr -> Maybe (Var -> (Process, Expr))
    lowerExpr (Old e) =
      Just $ \x -> (continuous x 0 e, Var x)
    lowerExpr (IntegralReset e reset) =
      Just $ \x ->
        let e' = Var x + Delta * e in
        (continuous x 0 e',
         -- x itself is the *old* value of the integral
         e')
    lowerExpr _ = Nothing

    replace e1 e2 p =
      transformBi (\e -> if e == e1 then e2 else e) p

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
eval _ _ (Bool x) =
  BoolValue x
eval delta env (Positive e) =
  BoolValue (double (eval delta env e) >= 0)
eval delta env (Zero e) =
  BoolValue (double (eval delta env e) == 0)
eval delta env (Cond e1 e2 e3) =
  if bool (eval delta env e1)
  then eval delta env e2
  else eval delta env e3
eval _ _ (Old _) =
  error "use 'lower' before evaluation"
eval _ _ (IntegralReset _ _) =
  error "use 'lower' before evaluation"

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
