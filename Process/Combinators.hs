module Process.Combinators where

import Process.Language
import qualified Data.Map.Strict as Map

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
initially, loop :: Step -> Process
initially p = process p skip
loop p = process skip p

-- A typeclass for things that can be executed in parallel
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
  Assume e s1 & s2 =
    Assume e (s1 & s2)
  Assert e s1 & s2 =
    Assert e (s1 & s2)
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
assume, assert :: Expr -> Step -> Step
assume = Assume
assert = Assert

-- Define a variable whose value changes with every step
continuous :: Var -> Expr -> Expr -> Process
continuous x start step =
  process (set x start) (set x step)

-- Choose between two processes. Initial state executes both
switch :: Expr -> Process -> Process -> Process
switch cond p1 p2 =
  combine (&) (If cond) p1 p2

----------------------------------------------------------------------
-- Combinators for building expressions
----------------------------------------------------------------------

instance Num Expr where
  fromInteger = Double . fromInteger
  x + y = Plus x y
  x * y = Times x y
  negate x = Negate x
  abs x = Cond (x >=? 0) x (negate x)
  signum x = Cond (x ==? 0) 0 (Cond (x >=? 0) 1 (-1))

instance Fractional Expr where
  fromRational = Double . fromRational
  recip x = Power x (-1)

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

deriv :: Expr -> Expr
deriv e = primitive Temporal "deriv" [e]

-- Desugarings for the primitives
stdPrims :: [(String, Prim)]
stdPrims =
  [("deriv",
    \[e] k ->
      -- old delta is the time different from the previous state to this
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

