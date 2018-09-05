{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternGuards, DeriveDataTypeable #-}
module Process where

import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Generics.Uniplate.Data
import Data.Tuple(swap)
import Data.Maybe
import Data.Ord
import Data.List
import Text.PrettyPrint.HughesPJClass hiding ((<>), double)
import qualified Text.PrettyPrint.HughesPJClass
import Text.Printf
import Utils
import Data.Data
import Control.Monad

----------------------------------------------------------------------
-- Processes
----------------------------------------------------------------------

-- A process consists of an initialisation step
-- followed by a loop step that runs repeatedly
data Process =
  Process {
    locals :: Int, -- number of bound variables
    start :: Step, -- initialisation
    step :: Step   -- loop step
  } deriving (Eq, Typeable, Data)

data Step =
   If Expr Step Step     -- if-then-else
 | Assume Expr Step      -- check precondition
 | Assert Expr Step      -- check postcondition
 | Update (Map Var Expr) -- update variables
 deriving (Eq, Typeable, Data)

data Expr =
   Var Var      -- variable
 | Delta        -- delta-t
   -- Arithmetic
 | Const Double -- constant
 | Plus Expr Expr
 | Times Expr Expr
 | Power Expr Expr
 | Negate Expr
 | Sin Expr
   -- Booleans
 | Not Expr
 | And Expr Expr
 | Bool Bool
 | Positive Expr -- e >= 0
 | Zero Expr     -- e == 0
   -- All these operations can be eliminated by calling 'lower'
 | Cond Expr Expr Expr
 | Min Expr Expr
 | Max Expr Expr
 | Old Expr Expr           -- previous value of expression; first argument: initial value
 | IntegralReset Expr Expr -- first argument: quantity to integrate; second argument: reset if true
 | Deriv Expr
 deriving (Eq, Ord, Typeable, Data)

data Var =
    Global String
  | Local Int
  deriving (Eq, Ord, Typeable, Data)

instance Show Process where
  show = show . pPrint

instance Show Step where
  show = show . pPrint

instance Show Expr where
  show = show . pPrint

instance Show Var where
  show = show . pPrint

----------------------------------------------------------------------
-- Free variables, renaming and replacement
----------------------------------------------------------------------

class Data a => Vars a where
  vars :: a -> Set Var
  vars = Set.fromList . universeBi

  rename :: (Var -> Var) -> a -> a
  rename = transformBi

instance Vars Expr
instance Vars Step
instance Vars Process

-- NOTE: Old has special semantics and we have to be careful about it.
--
-- Consider the following optimisation: replace
--   if e then C[e] else ...
-- with
--   if e then C[true] else ...
--
-- This optimisation is NOT CORRECT! If we have, say,
--   if e then ... Old e ... else ...
-- then we can certainly NOT replace Old e with Old true, because e may
-- not have been true in the previous timestep.
--
-- This mistake comes from thinking that two syntactically equal terms e1 and e2
-- must have the same value. But that's not true because Old changes the semantics
-- of its argument. The actual rule is:
--   Two syntactically equal terms t1 and t2 must have the same value,
--   IF they are nested under the same number of Olds.
--
-- So, when reasoning about equality of terms, you have to be careful about Old.
-- The two functions below try to take care of this for you in many cases.

-- Find all the subexpressions of a given value.
-- Each subexpression is wrapped in an appropriate number of Olds, for example:
--   exprs (Old (e1 + Old e2)) = [Old e1, Old (Old e2), ...]
-- This means that the returned subexpression, when evaluated outside of
-- any Olds, will have the same value as the original subexpression in its
-- original context.
exprs :: Data a => a -> [Expr]
exprs = concatMap f . childrenBi
  where
    -- See note above
    f (Old initial e) = f initial ++ map (Old initial) (f e)
    f e = e:concatMap f (children e)

-- Replace all occurrences of an expression with another one in a value.
-- Takes care to only replace occurrences that are nested in the correct
-- number of Olds. For example:
--   replace e1 e2 (Old e1) =
--     Old e1 (*not* Old e2)
--   replace (Old e1) (Old (Old e2)) (Old (e1 + e3)) =
--     Old (Old e2 + e3)
--   replace (Old e1) e2 (Old (e1 + e3)) =
--     e2 + Old e3 (since Old (e1 + e3) = Old e1 + Old e3)
-- If e1 is a subexpression returned by exprs e, then replace e1 e2 e is
-- guaranteed to replace that subexpression.
replace :: Data a => Expr -> Expr -> a -> a
replace e1 e2 = descendBi (f e1 e2)
  where
    f e1 e2 e | e == e1 = e2
    -- See note above
    f e1@(Old initial1 e1') e2@(Old initial2 e2') (Old initial e)
      | initial1 == initial =
        Old (f e1 e2 initial) (f e1' e2' e)
    -- This handles the case e.g. f (Old e1) e2 (Old (e1+x)) -> e2+Old x,
    -- which is necessary for compatibility with exprs
    f e1 e2 e@Old{} =
      case hideOld e of
        Old{} -> e
        e' ->
          let res = f e1 e2 e' in
          if e' == res then e else res
    f e1 e2 e = descend (f e1 e2) e

-- Move Old inwards so that the top-level constructor is not an Old,
-- unless the expression is Old^n (Var x) or Old^n Delta
hideOld :: Expr -> Expr
hideOld (Old initial Delta) = Old initial Delta
hideOld (Old initial (Var x)) = Old initial (Var x)
hideOld (Old initial e) = descend (Old initial) (hideOld e)
hideOld e = e

----------------------------------------------------------------------
-- Helper functions for the implementation
----------------------------------------------------------------------

-- Combine two processes
combine :: (Step -> Step -> Step) -> (Step -> Step -> Step) -> Process -> Process -> Process
combine startf stepf p q =
  Process {
    locals = locals p + locals q,
    start = startf (start p') (start q),
    step = stepf (step p') (step q) }
  where
    -- Shift p's locals so as not to clash with q
    -- (shifting p rather than q makes foldr combine take linear time)
    p' = rename shift p
    -- The guard here is necessary for the 'name' function to work
    shift (Local x) | x < locals p = Local (locals q+x)
    shift x = x

-- Map a function over steps
both :: (Step -> Step) -> Process -> Process
both f p = p { start = f (start p), step = f (step p) }

----------------------------------------------------------------------
-- Simple combinators for building processes
----------------------------------------------------------------------

instance Num Expr where
  fromInteger = Const . fromInteger
  x + y = Plus x y
  x * y = Times x y
  negate x = Negate x
  abs x = Cond (x >=? 0) x (negate x)
  signum x = Cond (x ==? 0) 0 (Cond (x >=? 0) 1 (-1))

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

-- Update a variable
set :: Var -> Expr -> Step
set x e = Update (Map.singleton x e)

-- Generate a local name
name :: (Var -> Process) -> Process
name f = p{locals = locals p + 1}
  where
    p = f xs
    xs = Local (locals p)

-- Define a variable whose value changes with every step
continuous :: Var -> Expr -> Expr -> Process
continuous x start step =
  process (set x start) (set x step)

-- Integral without reset
integral :: Expr -> Expr
integral e = IntegralReset e (Bool False)

-- Comparisons
infix 4 ==?, >=?, <=?, /=?
(==?), (>=?), (<=?) :: Expr -> Expr -> Expr
x ==? y = Zero (x-y)
x /=? y = Not (x ==? y)
x >=? y = Positive (x-y)
x <=? y = y >=? x

-- Booleans
orr :: Expr -> Expr -> Expr
orr e1 e2 = Not (And (Not e1) (Not e2))

-- Clamp a value to a range
clamp :: Expr -> Expr -> Expr -> Expr
clamp lo hi x =
  Max lo (Min hi x)

-- Choose between two processes. Initial state executes both
switch :: Expr -> Process -> Process -> Process
switch cond p1 p2 =
  combine (&) (If cond) p1 p2

----------------------------------------------------------------------
-- Simplifying processes and eliminating difficult constructs
----------------------------------------------------------------------

-- Do algebraic simplifications and similar
simplify :: Process -> Process
simplify = fixpoint (both simplifyStep)

simplifyStep :: Step -> Step
simplifyStep =
  fixpoint $
    transformBi simpStep .
    transformBi simplifyExpr
  where
    simpStep (If (Not e) s1 s2) = If e s2 s1
    simpStep (If (Bool True) e _) = e
    simpStep (If (Bool False) _ e) = e
    simpStep (If e s s') | s == s' = s
    simpStep (If e s1 s2) =
      If e
        (propagateBool e True s1)
        (propagateBool e False s2)
    simpStep (Assume (Bool True) s) = s
    simpStep (Assert (Bool True) s) = s
    simpStep s = s

simplifyExpr :: Expr -> Expr
simplifyExpr = fixpoint (transformBi simp)
  where
    simp e | Just x <- evalM Nothing Map.empty e = constant x
    simp (Old e1 e2)
      | Just x <- evalM Nothing Map.empty e1,
        Just y <- evalM Nothing Map.empty e2,
        e1 == e2 = constant x
    simp (Cond (Bool True) e _) = e
    simp (Cond (Bool False) _ e) = e
    simp (Cond _ e e') | e == e' = e
    simp (Cond cond e1 e2) =
      Cond cond (propagateBool cond True e1) (propagateBool cond False e2)
    simp (Not (Not e)) = e
    simp (Times (Const x) (Plus y z)) = Plus (Times (Const x) y) (Times (Const x) z)
    simp (Zero e)
      -- Sort u = t to t = u
      | neg > pos = Zero (Negate e)
      where
        (pos, neg) = terms e
    simp e@And{} =
      case conjuncts e of
        Nothing -> Bool False
        Just ([], []) -> Bool True
        Just (pos, neg) ->
          foldr1 And .
          -- Let each conjunct be simplified under the assumption
          -- that the others are true
          fixpoint (reverse . scanl1 propagate . reverse . scanl1 propagate) $
          pos ++ map Not neg
      where
        propagate e1 e2 =
          propagateBool e1 True e2
    simp e@Plus{} =
      case terms e of
        ([], []) -> Const 0
        (pos, neg) ->
          foldr1 Plus (pos ++ map Negate neg)
    simp e@Times{} =
      case factors e of
        (0, _) -> Const 0
        (k, []) -> Const k
        (k, es) ->
          -- If k is negative, multiply with abs k and use Negate
          -- (this simplifies better if inside a Plus)
          (if k < 0 then Negate else id) $
          -- Multiply with abs k only if necessary
          (if abs k == 1 then id else Times (Const (abs k))) $
          foldr1 Times es
    simp e = e

-- Given a Boolean expression and its truth value,
-- replace any other Boolean whose value can now be determined
-- (e.g. the same Boolean expression appearing elsewhere in the program)
propagateBool :: Data a => Expr -> Bool -> a -> a
propagateBool (Not e) val = propagateBool e (not val)
propagateBool (And e1 e2) True = propagateBool e1 True . propagateBool e2 True
propagateBool cond val = descendBi (propagate cond val)
  where
    propagate cond True e  | implies cond e = Bool True
    propagate cond False e | implies e cond = Bool False
    -- Keep track of the nesting level of Old.
    -- See note above 'exprs' for more info.
    propagate cond@(Old initial cond') val (Old initial' e)
      | initial == initial' =
        Old (propagate cond val initial')
            (propagate cond' val e)
    propagate cond val (Old initial e) = Old initial e
    propagate cond val e = descend (propagate cond val) e

    -- This is pretty crappy but helps with abs somewhat
    -- (no need for Zero/Zero case because simplify will normalise)
    implies (Zero e1) (Positive e2)
      | (k, [], []) <- terms' (e2 - e1),
        k >= 0 = True
    implies (Zero e1) (Positive e2)
      | (k, [], []) <- terms' (e2 + e1),
        k >= 0 = True
    implies (Positive e1) (Positive e2)
      | (k, [], []) <- terms' (e2 - e1),
        k >= 0 = True
    implies e1 (And e2 e3) =
      implies e1 e2 && implies e1 e3
    implies (And e1 e2) e3 =
      implies e1 e3 || implies e2 e3
    implies e1 e2 = e1 == e2

-- Eliminate difficult constructs
lower :: Process -> Process
lower = fixpoint (eliminateState . eliminateCond . eliminateMinMax . eliminateDeriv . simplify)

eliminateState :: Process -> Process
eliminateState =
  -- Idea: replace an expression such as an integral with a process that
  -- computes the integral and stores the result in a variable
  -- e.g.:
  --   x := .... integral e ....
  -- becomes
  --   (x := .... v ....) & continuous v 0 (v + e * delta)
  fixpoint $ \p ->
    case [(e, f) | e <- exprs p, f <- maybeToList (lowerExpr e)] of
      [] -> p
      ((e, f):_) ->
        name $ \x ->
          let
            (q, e') = f x
          in
            replace e e' p & q
  where
    -- Input: expression to lower
    -- Output:
    --   Nothing if cannot be lowered
    --   Otherwise Just (\x -> (p, e)) where:
    --     * x is the variable the expression will be stored in
    --     * p is a process which will compute x
    --     * e is the lowering of the expression
    lowerExpr :: Expr -> Maybe (Var -> (Process, Expr))
    lowerExpr (Old initial e) =
      Just $ \x ->
      (continuous x initial e,
       -- e.g. y := Old e becomes
       --      y := x & x := e
       -- which has the right effect because all updates are performed
       -- simultaneously
       Var x)
    lowerExpr (IntegralReset e reset) =
      Just $ \x ->
        let e' = Cond reset 0 (Var x + Delta * e) in
        (continuous x 0 e',
         -- x itself is the *old* value of the integral,
         -- so e' is the current value
         e')
    lowerExpr _ = Nothing

eliminateCond :: Process -> Process
eliminateCond =
  fixpoint (both (transformBi f . transformBi introBool . simplifyStep))
  where
    -- Idea: replace
    --   ... Cond cond e1 e2 ...
    -- with
    --   if cond then [... Cond cond e1 e2 ...] else [... Cond cond e1 e2 ...]
    -- and then use propagateBool to get rid of the Cond in each branch
    f s =
      case findConds s of
        [] -> s
        cond:_ ->
          If cond
            (propagateBool cond True s)
            (propagateBool cond False s)
    -- If Cond is the argument to a predicate (And, Not, Zero, Positive),
    -- encode it using Boolean connectives:
    --   P(Cond e1 e2 e3) = (e1 && P(e2)) || (not e1 && P(e3))
    -- Actually, we produce:
    --   (e1 && P(Cond e1 e2 e3)) || (not e1 && P(Cond e1 e2 e3))
    -- and leave it to propagateBool to simplify.
    introBool e
      | isBool e, cond:_ <- findConds e =
         orr
           (And cond (propagateBool cond True e))
           (And (Not cond) (propagateBool cond False e))
      | otherwise = e

    isBool And{} = True
    isBool Not{} = True
    isBool Zero{} = True
    isBool Positive{} = True
    isBool _ = False

    findConds e =
      [cond | Cond cond _ _ <- map hideOld (exprs e)]

eliminateMinMax :: Process -> Process
eliminateMinMax = transformBi f
  where
    f (Min e1 e2) = Cond (e1 >=? e2) e2 e1
    f (Max e1 e2) = Cond (e1 >=? e2) e1 e2
    f e = e

eliminateDeriv :: Process -> Process
eliminateDeriv = transformBi f
  where
    f (Deriv e) = (e - Old 0 e) * Old 0 (1 / Delta)
    f e = e

-- Separates a sum into positive and negative parts
terms :: Expr -> ([Expr], [Expr])
terms e
  | k > 0  = (Const k:pos, neg)
  | k < 0  = (pos, Const (-k):neg)
  | k == 0 = (pos, neg)
  where
    (k, pos, neg) = terms' e

-- Separates a sum into positive and negative parts and a constant
terms' :: Expr -> (Double, [Expr], [Expr])
terms' e = (k, pos \\ neg, neg \\ pos) -- remove common terms
  where
    (k, pos, neg) = go e

    go (Plus e1 e2) = (k1 + k2, pos1 ++ pos2, neg1 ++ neg2)
      where
        (k1, pos1, neg1) = go e1
        (k2, pos2, neg2) = go e2
    go (Negate e) = (-k, neg, pos)
      where
        (k, pos, neg) = go e
    go (Const x) = (x, [], [])
    go e = (0, [e], [])

-- Separates a product into constant and non-constant parts
factors :: Expr -> (Double, [Expr])
factors (Times e1 e2) = (k1*k2, xs ++ ys)
  where
    (k1, xs) = factors e1
    (k2, ys) = factors e2
factors (Negate e) = (negate k, xs)
  where
    (k, xs) = factors e
factors (Const x) = (x, [])
factors e = (1, [e])

-- Separates a conjunction into its conjuncts.
-- Returns Nothing if contradictory.
conjuncts :: Expr -> Maybe ([Expr], [Expr])
conjuncts e = do
  (pos, neg) <- conj e
  guard (null (intersect pos neg))
  return (usort pos, usort neg)
  where
    conj (And e1 e2) = do
      (pos1, neg1) <- conj e1
      (pos2, neg2) <- conj e2
      return (pos1 ++ pos2, neg1 ++ neg2)
    conj (Not e) = return ([], [e])
    conj (Bool True) = return ([], [])
    conj (Bool False) = Nothing
    conj e = return ([e], [])

----------------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------------

type Env = Map Var Value
data Value = DoubleValue Double | BoolValue Bool deriving (Eq, Show)

double :: Monad m => Value -> m Double
double (DoubleValue x) = return x
double _ = fail "type error: got bool but expected double"

bool :: Monad m => Value -> m Bool
bool (BoolValue x) = return x
bool _ = fail "type error: got double but expected bool"

constant :: Value -> Expr
constant (DoubleValue x) = Const x
constant (BoolValue x) = Bool x

eval :: Double -> Env -> Expr -> Value
eval delta env e =
  case evalM (Just delta) env e of
    Left err -> error ("eval: " ++ err)
    Right x -> x

-- The evaluator is monadic so that we can use it
-- in simplify for constant folding
evalM :: Monad m => Maybe Double -> Env -> Expr -> m Value
evalM _ env (Var x) =
  case Map.lookup x env of
    Nothing -> fail ("variable " ++ show x ++ " not bound")
    Just v -> return v
evalM mdelta _ Delta =
  case mdelta of
    Nothing -> fail "delta not defined"
    Just delta -> return (DoubleValue delta)
evalM _ _ (Const x) =
  return (DoubleValue x)
evalM delta env (Plus e1 e2) =
  DoubleValue <$> do
    x <- double =<< evalM delta env e1
    y <- double =<< evalM delta env e2
    return (x+y)
evalM delta env (Times e1 e2) =
  DoubleValue <$> do
    x <- double =<< evalM delta env e1
    y <- double =<< evalM delta env e2
    return (x*y)
evalM delta env (Power e1 e2) =
  DoubleValue <$> do
    x <- double =<< evalM delta env e1
    y <- double =<< evalM delta env e2
    return (x**y)
evalM delta env (Negate e) =
  DoubleValue <$> negate <$> (double =<< evalM delta env e)
evalM delta env (Sin e) =
  DoubleValue <$> sin <$> (double =<< evalM delta env e)
evalM delta env (Not e) =
  BoolValue <$> not <$> (bool =<< evalM delta env e)
evalM delta env (And e1 e2) =
  BoolValue <$> do
    x <- bool =<< evalM delta env e1
    y <- bool =<< evalM delta env e2
    return (x && y)
evalM _ _ (Bool x) =
  return (BoolValue x)
evalM delta env (Positive e) =
  BoolValue <$> (>= 0) <$> (double =<< evalM delta env e)
evalM delta env (Zero e) =
  BoolValue <$> (== 0) <$> (double =<< evalM delta env e)
evalM delta env (Cond e1 e2 e3) = do
  x <- bool =<< evalM delta env e1
  if x then
    evalM delta env e2
  else
    evalM delta env e3
evalM delta env (Min e1 e2) =
  DoubleValue <$> do
    x <- double =<< evalM delta env e1
    y <- double =<< evalM delta env e2
    return (min x y)
evalM delta env (Max e1 e2) =
  DoubleValue <$> do
    x <- double =<< evalM delta env e1
    y <- double =<< evalM delta env e2
    return (max x y)
evalM _ _ e =
  fail ("dont't know how to evaluate " ++ show e)

data Result = OK | PreconditionFailed Expr | PostconditionFailed Expr
  deriving Show

execStep :: Double -> Env -> Step -> (Env, Result)
execStep delta env (If e s1 s2) =
  if fromJust (bool (eval delta env e))
  then execStep delta env s1
  else execStep delta env s2
execStep delta env (Assume e s) =
  if fromJust (bool (eval delta env e))
  then execStep delta env s
  else (env, PreconditionFailed e)
execStep delta env (Assert e s) =
  if fromJust (bool (eval delta env e))
  then execStep delta env s
  else (env, PostconditionFailed e)
execStep delta env (Update m) =
  -- N.B. Map.union is left-biased
  (Map.union (Map.map (eval delta env) m) env, OK)

simulate :: Double -> [Env] -> Process -> ([Env], Result)
simulate delta inputs process =
  loop Map.empty [] (Map.empty:inputs) (start process':repeat (step process'))
  where
    process' = lower process

    loop _ history [] _ = (reverse history, OK)
    loop env history (inp:inps) (step:steps) =
      case execStep delta (Map.union inp env) step of
        (env, OK) -> loop env (env:history) inps steps
        (env, err) -> (reverse (env:history), err)

----------------------------------------------------------------------
-- Pretty-printing
----------------------------------------------------------------------

instance Pretty Process where
  pPrint p =
    vcat $
      [ hang (text "start") 2 (pPrint (start p)) | nonempty (start p) ] ++
      [ hang (text "step") 2 (pPrint (step p)) | nonempty (step p) ]
    where
      nonempty (Update m) = Map.size m > 0
      nonempty _ = True

instance Pretty Step where
  pPrint (If e s1 s2) =
    ppIfThenElse (pPrint e) (pPrint s1) (pPrint s2)
  pPrint (Assume e s) =
    hang (text "assume") 2 (pPrint e) $$
    pPrint s
  pPrint (Assert e s) =
    hang (text "e") 2 (pPrint e) $$
    pPrint s
  pPrint (Update m)
    | Map.null m = text "skip"
    | otherwise =
      vcat
        [ hang (pPrint x <+> text "<-") 2 (pPrint e)
        | (x, e) <- Map.toList m ]

instance Pretty Expr where
  pPrintPrec _ p = ppExp p

instance Pretty Var where
  pPrint (Global x) = text x
  pPrint (Local n) = text "x" <#> pPrint n

instance Pretty Doc where
  pPrint = id

-- Precedence levels:
-- 0: no brackets
-- 1: and (associative)
-- 2: not
-- 3: positive/zero
-- 4: plus (associative)
-- 5: negate
-- 6: times (associative)
-- 7: power (non-associative)
-- 9: atomic
ppExp :: Rational -> Expr -> Doc
ppExp _ (Var x) = pPrint x
ppExp _ Delta = text "dt"
ppExp _ (Const x) = text (shortest (show x) (printf "%.5f" x))
  where
    shortest x y
      | length x <= length y = x
      | otherwise = y
ppExp n (Plus e1 e2) =
  maybeParens (n > 4) $
    sep [
      ppExp 4 e1,
      -- a + -b => a-b
      -- a + (-b+c) => a - b + c
      case e2 of
        Negate e3 ->
          text "-" <+> ppExp 5 e3
        Plus (Negate e3) e4 ->
          text "-" <+> ppExp 4 (Plus e3 e4)
        _ ->
          text "+" <+> ppExp 4 e2]
ppExp n (Times e1 e2) =
  ppAssoc n "*" 6 e1 e2
ppExp n (Power e (Const (-1))) =
  ppUnary n "1/" 7 e
ppExp n (Power e1 e2) =
  maybeParens (n > 7) $
    cat [ppExp 8 e1 <#> text "^", nest 2 (ppExp 8 e2)]
ppExp n (Negate e) = ppUnary n "-" 5 e
ppExp _ (Sin e) = ppFunction "sin" [e]
ppExp n (Not (And e1 e2)) =
  ppNonAssoc n "or" 1 (neg e1) (neg e2)
  where
    neg (Not x) = x
    neg x = Not x
ppExp n (Not e) = ppUnary n "not " 2 e
ppExp n (And e1 e2) = ppNonAssoc n "and" 1 e1 e2
ppExp _ (Bool True) = text "true"
ppExp _ (Bool False) = text "false"
ppExp n (Positive e) =
  ppAssoc n ">=" 3 (ppSum pos) (ppSum neg)
  where
    (pos, neg) = terms e
ppExp n (Zero e) =
  ppAssoc n "=" 3 (ppSum pos) (ppSum neg)
  where
    (pos, neg) = terms e
ppExp n (Cond e1 e2 e3) =
  maybeParens (n > 0) $
    -- else-branch must be atomic to avoid ambiguity
    ppIfThenElse (ppExp 0 e1) (ppExp 0 e2) (ppExp 9 e3)
ppExp n (Min e1 e2) =
  ppFunction "min" [e1, e2]
ppExp n (Max e1 e2) =
  ppFunction "max" [e1, e2]
ppExp n (Old initial e) =
  ppFunction "old" [initial, e]
ppExp n (IntegralReset e (Bool False)) =
  ppFunction "integral" [e]
ppExp n (IntegralReset e reset) =
  ppFunction "integral" [e, reset]
ppExp n (Deriv e) =
  ppFunction "derivative" [e]

ppFunction :: String -> [Expr] -> Doc
ppFunction op es =
  cat [
    text op,
    nest 2 $ parens $
      sep (punctuate comma (map (ppExp 0) es))]

ppUnary :: Rational -> String -> Rational -> Expr -> Doc
ppUnary n op p e =
  maybeParens (n > p) $
    text op <#> ppExp (p+1) e

ppAssoc :: Rational -> String -> Rational -> Expr -> Expr -> Doc
ppAssoc n op p e1 e2 =
  maybeParens (n > p) $
    sep [ppExp p e1, text op <+> ppExp p e2]

ppNonAssoc :: Rational -> String -> Rational -> Expr -> Expr -> Doc
ppNonAssoc n op p e1 e2 =
  maybeParens (n > p) $
    sep [ppExp (p+1) e1, text op <+> ppExp (p+1) e2]

ppSum :: [Expr] -> Expr
ppSum [] = Const 0
ppSum xs = foldr1 (+) xs

ppIfThenElse :: Doc -> Doc -> Doc -> Doc
ppIfThenElse x y z =
  sep [
    sep [text "if", nest 2 x, text "then"],
    nest 2 y,
    text "else",
    nest 2 z]

infixl 6 <#>
(<#>) :: Doc -> Doc -> Doc
(<#>) = (Text.PrettyPrint.HughesPJClass.<>)
