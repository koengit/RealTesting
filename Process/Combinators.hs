-- Combinators for building processes and expressions.
module Process.Combinators where

import Process.Language
import Process.Simplify
import qualified Data.Map.Strict as Map
import Data.Generics.Uniplate.Data
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Utils

----------------------------------------------------------------------
-- Combinators for building processes.
----------------------------------------------------------------------

-- Make a simple process
process :: Step -> Step -> Process
process start step =
  Process {
    locals = 0,
    start = start,
    step = step }

-- Do something only on the first step, or only after the first step
first, loop :: Step -> Process
first p = process p skip
loop p = process skip p

-- Parallel composition
class Par a where
  -- A process that does nothing
  skip :: a
  -- Execute two processes in parallel
  (&) :: a -> a -> a

infixr 5 &

-- Execute many processes in parallel
par :: Par a => [a] -> a
par = foldr (&) skip

instance Par Step where
  skip = Update Map.empty

  If e s1 s2 & s3 =
    If e (s1 & s3) (s2 & s3)
  Assume str e s1 & s2 =
    Assume str e (s1 & s2)
  Assert str e s1 & s2 =
    Assert str e (s1 & s2)
  Update m1 & Update m2 =
    Update (Map.unionWith f m1 m2)
    where
      f x y
        | x == y = x
        | otherwise = error "Step.&: incoherent state update"
  s1@Update{} & s2 = s2 & s1

instance Par Process where
  skip = process skip skip
  (&) = combine (&) (&)

-- Generate a local name
name :: (Var -> Process) -> Process
name f = p{locals = locals p + 1}
  where
    p = f n
    n = Local (locals p)

-- Update a variable
set :: Var -> Expr -> Step
set x e = Update (Map.singleton x e)

-- If-then-else
ite :: Expr -> Step -> Step -> Step
ite = If

-- Assumptions and assertions
assume, assert :: String -> Expr -> Step
assume str e = Assume str e skip
assert str e = Assert str e skip

-- Define a variable whose value changes with every step
continuous :: Var -> Expr -> Expr -> Process
continuous x start step =
  process (set x start) (set x step)

-- Choose between two processes. Initial state executes both
switch :: Expr -> Process -> Process -> Process
switch cond p1 p2 =
  combine (&) (If cond) p1 p2

-- Sequential composition
sequential :: Process -> Expr -> Process -> Process
sequential p e q =
  name $ \x ->
    combine
      (\_ _ -> (start p & set x (bool False)))
      (\_ _ ->
        (ite (var x)
          (step q)
          (ite e (set x (bool True) & start q)
            (step p))))
      p q

wait :: Expr -> Process -> Process
wait e p = sequential skip e p

----------------------------------------------------------------------
-- Combinators for building expressions
----------------------------------------------------------------------

var :: Var -> Expr
var = Var

delta :: Expr
delta = Delta

double :: Double -> Expr
double = Double

nott :: Expr -> Expr
nott = Not

infixr 3 &&&
(&&&) :: Expr -> Expr -> Expr
(&&&) = And

infixr 2 |||
(|||) :: Expr -> Expr -> Expr
(|||) x y = nott (nott x &&& nott y)

infixr 0 ==>
(==>) :: Expr -> Expr -> Expr
x ==> y = nott x ||| y

bool :: Bool -> Expr
bool = Bool

true, false :: Expr
true = bool True
false = bool False

infix 4 ==?, >=?, <=?, /=?
(==?), (>=?), (<=?) :: Expr -> Expr -> Expr
x ==? y = Zero (x-y)
x /=? y = Not (x ==? y)
x >=? y = Positive (x-y)
x <=? y = y >=? x

cond :: Expr -> Expr -> Expr -> Expr
cond = Cond

-- Primitive functions
primitive :: PrimitiveKind -> String -> [Expr] -> Expr
primitive = Primitive

minn, maxx :: Expr -> Expr -> Expr
minn x y = primitive Functional "min" [x, y]
maxx x y = primitive Functional "max" [x, y]

old :: Expr -> Expr -> Expr
old initial x = primitive Temporal "old" [initial, x]

integralReset :: Expr -> Expr -> Expr
integralReset e reset = primitive Temporal "integral" [e, reset]

integral :: Expr -> Expr
integral e = integralReset e false

smartIntegral :: Expr -> Expr
smartIntegral = linear f
  where
    f (Primitive Temporal "deriv" [e]) = e
    f e = integral e

deriv :: Expr -> Expr
deriv e = primitive Temporal "deriv" [e]

derivDegree :: Var -> Expr -> Int
derivDegree x e =
  maximum (mapMaybe deg (universeBi e))
  where
    deg (Var y) | x == y = Just 0
    deg (Primitive Temporal "deriv" [e]) = fmap succ (deg e)
    deg _ = Nothing

linear :: (Expr -> Expr) -> Expr -> Expr
linear f (Plus e1 e2) = Plus (linear f e1) (linear f e2)
linear f (Negate e) = Negate (linear f e)
linear f (Times (Double k) e) = Times (Double k) (linear f e)
linear f (Times e (Double k)) = Times (linear f e) (Double k)
linear f e = f e

-- Desugarings for the primitives
stdPrims :: [(String, Prim)]
stdPrims =
  [("deriv",
    \[e] k ->
      -- old delta is the time difference from the previous state to this
      k ((e - old 0 e) * old 0 (1 / delta))),
   ("integral",
    \[e, reset] k ->
      name $ \x ->
        let e' = cond reset 0 (var x + delta * e) in
        continuous x 0 e' &
        -- x is the *old* value of the integral, so e' is the current value
        k e'),
   ("old",
    \[initial, e] k ->
      name $ \x ->
        -- Works because all updates are done simultaneously
        continuous x initial e & k (Var x)),
   ("min",
    \[x, y] k -> k (Cond (x <=? y) x y)),
   ("max",
    \[x, y] k -> k (Cond (x >=? y) x y))]

-- Clamp a value to a range
clamp :: Expr -> Expr -> Expr -> Expr
clamp lo hi x = maxx lo (minn hi x)

-- Define a variable by means of a differential equation in t.
differentialEquation :: Var -> Expr -> Expr -> Process
differentialEquation x initial e =
  continuous x initial (solve (Var x) (foldn (derivDegree x e) smartIntegral e))

-- Given e = L(y)/L(x),
-- transferFunction y x s e finds the differential equation defining y.
transferFunction :: Var -> Var -> Var -> Expr -> Expr
transferFunction y x s e =
  -- L(y)/L(x) = k*num/denom
  -- L(y)*denom = k*L(x)*num
  inverseLaplace s (var y * product denom - Double k * var x * product num)
  where
    (k, num, denom) = factors' e

-- Given e = L(x),
-- laplaceFunction x s e finds the differential equation defining x.
laplaceFunction :: Var -> Var -> Expr -> Expr
laplaceFunction x s e =
  replaceGlobal (deriv (Double 0)) (Double 0) $
  replaceGlobal (deriv (Double 1)) (Double 0) $
  replaceGlobal (deriv (var t)) (Double 1) $
  transferFunction x t s e
  where
    t = Global "laplaceT"

-- Try to invert the Laplace transform. Only handles differential equations.
inverseLaplace :: Var -> Expr -> Expr
inverseLaplace s = simplifyExpr . linear eliminate . expand . simplifyExpr
  where
    eliminate e
      | s `Set.notMember` vars e = e
      | var s `elem` es =
        Double k * deriv (eliminate (product (es \\ [var s])))
      | otherwise = error "couldn't eliminate s from Laplace transform"
      where
        (k, es) = factors e
