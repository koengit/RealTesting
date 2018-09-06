{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Process(module Process, Var(..), Par(..), par, Value(..), Env, Valued(..), Result(..)) where

import Process.Core(Var(..), Par(..), par)
import qualified Process.Core as Core
import qualified Process.Simplify as Simplify
import qualified Process.Eval as Eval
import Process.Eval(Value(..), Env, Valued(..), Result(..))
import qualified Data.Map.Strict as Map
import Control.Monad

type System = Core.Process
type Process = Core.Named Core.Process
type Expr = Core.Named Core.Expr
type Step = Core.Named Core.Step

compile :: Process -> System
compile p = Core.compileNamed p id

simplify :: System -> System
simplify = Simplify.simplify

type Prim = [Expr] -> Expr

lower :: [(String, Prim)] -> System -> System
lower prims =
  Simplify.lower [(name, transform prim) | (name, prim) <- prims]
  where
    transform f es = f (map return es)

standardPrims :: [(String, Prim)]
standardPrims =
  [("deriv", \[e] -> (e - old 0 e) * old 0 (1 / delta)),
   ("integral",
    \[e, reset] ->
      letProcess $ \x ->
        let e' = ite reset 0 (var x + delta * e) in
        (continuous x 0 e',
         -- x itself is the *old* value of the integral,
         -- so e' is the current value
         e'))]

simulate :: Valued f => Double -> [Env] -> System -> f ([Env], Result)
simulate delta inputs system =
  Eval.simulate delta inputs (lower standardPrims system)

instance Num Expr where
  fromInteger = return . fromInteger
  (+) = liftM2 (+)
  (*) = liftM2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

instance Fractional Expr where
  fromRational = return . fromRational
  recip = fmap recip

var :: Var -> Expr
var = return . Core.Var

delta :: Expr
delta = return Core.Delta

double :: Double -> Expr
double = return . Core.Double

nott :: Expr -> Expr
nott = fmap Core.Not

andd :: Expr -> Expr -> Expr
andd = liftM2 Core.And

orr :: Expr -> Expr -> Expr
orr e1 e2 = nott (nott e1 `andd` nott e2)

bool :: Bool -> Expr
bool = return . Core.Bool

true, false :: Expr
true = bool True
false = bool False

infix 4 ==?, >=?, <=?, /=?
(==?), (/=?), (>=?), (<=?) :: Expr -> Expr -> Expr
(==?) = liftM2 (Core.==?)
(/=?) = liftM2 (Core./=?)
(>=?) = liftM2 (Core.>=?)
(<=?) = liftM2 (Core.<=?)

ite :: Expr -> Expr -> Expr -> Expr
ite = liftM3 Core.Ite

minn, maxx :: Expr -> Expr -> Expr
minn = liftM2 Core.Min
maxx = liftM2 Core.Max

integralReset :: Expr -> Expr -> Expr
integralReset e reset = primitive "integral" [e, reset]

integral :: Expr -> Expr
integral e = integralReset e false

primitive :: String -> [Expr] -> Expr
primitive name = fmap (Core.Primitive name) . sequence

deriv :: Expr -> Expr
deriv e = primitive "deriv" [e]

-- Clamp a value to a range
clamp :: Expr -> Expr -> Expr -> Expr
clamp lo hi x = maxx lo (minn hi x)

letProcess :: (Var -> (Process, Expr)) -> Expr
letProcess f =
  Core.Name $ \x -> do
    let (p, e) = f x
    p' <- p
    e' <- e
    return (p', e')

-- Get the previous value of an expression
old :: Expr -> Expr -> Expr
old initial e = letProcess (\x -> (continuous x initial e, var x))

----------------------------------------------------------------------
-- Combinators for building processes
----------------------------------------------------------------------

-- Make a simple process
process :: Step -> Step -> Process
process = liftM2 Core.process

-- Update a variable
set :: Var -> Expr -> Step
set x e = do
  ex <- e
  return (Core.Update (Map.singleton x ex))

-- Define a variable whose value changes with every step
continuous :: Var -> Expr -> Expr -> Process
continuous x start step =
  process (set x start) (set x step)

-- Choose between two processes. Initial state executes both
switch :: Expr -> Process -> Process -> Process
switch cond p1 p2 = liftM3 sw cond p1 p2
  where
    sw cond p1 p2 = Core.combine (&) (Core.If cond) p1 p2
