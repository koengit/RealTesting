{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternGuards #-}
module Process where

import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Generics.Geniplate -- cabal install geniplate-mirror
import Data.Tuple(swap)
import Data.Maybe
import Data.Ord
import Data.List
import Text.PrettyPrint.HughesPJClass hiding ((<>), double)
import qualified Text.PrettyPrint.HughesPJClass
import Text.Printf

infixl 6 <#>
(<#>) :: Doc -> Doc -> Doc
(<#>) = (Text.PrettyPrint.HughesPJClass.<>)

data Var =
    Global String
  | Local Int
  deriving (Eq, Ord)

instance Show Var where
  show = show . pPrint

instance Pretty Var where
  pPrint (Global x) = text x
  pPrint (Local n) = text "x" <> pPrint n

-- A process consists of an initialisation step
-- followed by a loop step that runs repeatedly
data Process =
  Process {
    locals :: Int, -- number of bound variables
    start :: Step, -- initialisation
    step :: Step   -- loop step
  } deriving Eq

instance Show Process where
  show = show . pPrint

instance Pretty Process where
  pPrint p =
    hang (text "start") 2 (pPrint (start p)) $$
    hang (text "step") 2 (pPrint (step p))

data Step =
   If Expr Step Step     -- if-then-else
 | Assume Expr Step      -- check precondition
 | Assert Expr Step      -- check postcondition
 | Update (Map Var Expr) -- update variables
 deriving Eq

instance Show Step where
  show = show . pPrint

instance Pretty Step where
  pPrint (If e s1 s2) =
    ppIfThenElse (pPrint e) (pPrint s1) (pPrint s2)
  pPrint (Assume e s) =
    hang (text "assume") 2 (pPrint e) $$
    pPrint s
  pPrint (Assert e s) =
    hang (text "e") 2 (pPrint e) $$
    pPrint s
  pPrint (Update m) =
    vcat
      [ hang (pPrint x <+> text "<-") 2 (pPrint e)
      | (x, e) <- Map.toList m ]

ppIfThenElse :: Doc -> Doc -> Doc -> Doc
ppIfThenElse x y z =
  sep [
    sep [sep [text "if", nest 2 x, text "then"], nest 2 y, text "else"],
      nest 2 z]

data Expr =
   Const Double -- constant
 | Var Var -- variable
 | Delta   -- delta-t
   -- Arithmetic
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
 | Old Expr
 | IntegralReset Expr Expr -- first argument: quantity to integrate; second argument: reset if true
 | Deriv Expr
 deriving (Eq, Ord)

instance Show Expr where
  show = show . pPrint

instance Pretty Expr where
  pPrintPrec _ p = ppExp p

instance Pretty Doc where
  pPrint = id

-- Precedence levels:
-- 0: no brackets
-- 1: and (associative)
-- 2: not
-- 3: positive/zero, min/max (whose arguments get precedence 9)
-- 4: plus (associative)
-- 5: negate
-- 6: times (associative)
-- 7: power (non-associative)
-- 8: sin
-- 9: atomic
ppExp :: Rational -> Expr -> Doc
ppExp _ (Var x) = pPrint x
ppExp _ Delta = text "dt"
ppExp _ (Const x) = text (shortest (show x) (printf "%.5f" x))
  where
    shortest x y
      | length x <= length y = x
      | otherwise = y
ppExp n e@Plus{}
  | null neg =
    maybeParens (n > 4) $
      foldr1 (assoc 0 " + " 4) (map (ppExp 4) pos)
  | otherwise =
    maybeParens (n > 4) $
      foldr (assoc 0 " + " 4) (foldr1 (assoc 0 " - " 4) (map (ppExp 4) neg)) pos
  where
    (pos, neg) = terms e
ppExp n (Times e1 e2) = assoc n " * " 6 e1 e2
ppExp n (Power e (Const (-1))) = unary n "1/" 7 e
ppExp n (Power e1 e2) = nonassoc n "^" 7 e1 e2
ppExp n (Negate e) = unary n "-" 5 e
ppExp n (Sin e) = unary n "sin " 8 e
ppExp n (Not e) = unary n "not " 2 e
ppExp n (And e1 e2) = assoc n " and " 1 e1 e2
ppExp _ (Bool True) = text "true"
ppExp _ (Bool False) = text "false"
ppExp n (Positive e) =
  assoc n " >= " 3 (plus pos) (plus neg)
  where
    (pos, neg) = terms e
ppExp n (Zero e) =
  assoc n " = " 3 (plus pos) (plus neg)
  where
    (pos, neg) = terms e
ppExp n (Cond e1 e2 e3) =
  maybeParens (n > 0) $
    -- else-branch must be atomic to avoid ambiguity
    ppIfThenElse (ppExp 0 e1) (ppExp 0 e2) (ppExp 9 e3)
ppExp n (Min e1 e2) =
  maybeParens (n > 3) $
    hang (ppExp 9 e1 <+> text "min") 2 (ppExp 9 e2)
ppExp n (Max e1 e2) =
  maybeParens (n > 3) $
    hang (ppExp 9 e1 <+> text "max") 2 (ppExp 9 e2)
ppExp n (Old e) =
  maybeParens (n > 9) $
    hang (text "old") 2 (ppExp 9 e)
ppExp n (IntegralReset e (Bool False)) =
  maybeParens (n > 9) $
    hang (text "integral") 2 (ppExp 9 e)
ppExp n (IntegralReset e reset) =
  maybeParens (n > 0) $
    sep [
      hang (text "integral") 2 (ppExp 9 e),
      hang (text "reset") 2 (ppExp 9 reset)]
ppExp n (Deriv e) =
  maybeParens (n > 9) $
    hang (text "deriv") 2 (ppExp 9 e)

plus :: [Expr] -> Expr
plus [] = Const 0
plus xs = foldr1 Plus xs

terms :: Expr -> ([Expr], [Expr])
terms (Plus e1 e2) = (pos1 ++ pos2, neg1 ++ neg2)
  where
    (pos1, neg1) = terms e1
    (pos2, neg2) = terms e2
terms (Negate e) = swap (terms e)
terms (Const x)
  | x >= 0 = ([Const x], [])
  | otherwise = ([], [Const (negate x)])
terms e = ([e], [])

unary :: Pretty a => Rational -> String -> Rational -> a -> Doc
unary n name p e =
  maybeParens (n > p) $
    text name <> pPrintPrec prettyNormal (p+1) e

assoc :: (Pretty a, Pretty b) => Rational -> String -> Rational -> a -> b -> Doc
assoc n name p e1 e2 =
  maybeParens (n > p) $
    cat [
      pPrintPrec prettyNormal p e1 <> text name,
      pPrintPrec prettyNormal p e2]

nonassoc :: (Pretty a, Pretty b) => Rational -> String -> Rational -> a -> b -> Doc
nonassoc n name p e1 e2 =
  maybeParens (n > p) $
    cat [
      pPrintPrec prettyNormal (p+1) e1 <> text name,
      pPrintPrec prettyNormal (p+1) e2]

----------------------------------------------------------------------
-- free variables of expressions

instanceUniverseBi [t| (Expr, Var)|]
instanceUniverseBi [t| (Step, Var)|]
instanceUniverseBi [t| (Process, Var)|]
instanceUniverseBi [t| (Expr, Expr)|]
instanceUniverseBi [t| (Step, Expr)|]
instanceUniverseBi [t| (Process, Expr)|]
instanceUniverseBi [t| (Step, Map Var Expr)|]
instanceTransformBi [t| (Expr, Expr)|]
instanceTransformBi [t| (Expr, Step)|]
instanceTransformBi [t| (Expr, Process)|]
instanceTransformBi [t| (Var, Expr)|]
instanceTransformBi [t| (Var, Step)|]
instanceTransformBi [t| (Var, Process)|]
instanceTransformBi [t| (Step, Step)|]

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

both :: (Step -> Step) -> Process -> Process
both f p = p { start = f (start p), step = f (step p) }

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

-- Integral without reset
integral :: Expr -> Expr
integral e = IntegralReset e (Bool False)

-- Comparisons
(==?), (>=?), (<=?) :: Expr -> Expr -> Expr
x ==? y = Zero (x-y)
x >=? y = Positive (x-y)
x <=? y = y >=? x

-- Clamp a value to a range
clamp :: Expr -> Expr -> Expr -> Expr
clamp lo hi x =
  Max lo (Min hi x)

-- Choose between two processes. Initial state executes both
switch :: Expr -> Process -> Process -> Process
switch cond p1 p2 =
  combine parS2 (If cond) p1 p2

----------------------------------------------------------------------
-- lowering operations

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
  | x == y = x
  | otherwise = fixpoint f y
  where
    y = f x

simplify :: Process -> Process
simplify p = both simplifyS p

simplifyS :: Step -> Step
simplifyS =
  fixpoint $
    transformBi simpStep .
    transformBi simpExpr
  where
    simpStep (If (Bool True) e _) = e
    simpStep (If (Bool False) _ e) = e
    simpStep (Assume (Bool True) s) = s
    simpStep (Assert (Bool True) s) = s
    simpStep s = s

    simpExpr e | Just x <- constantValue e = constant x
    simpExpr (Old e) | Just x <- constantValue e = constant x
    simpExpr (Cond (Bool True) e _) = e
    simpExpr (Cond (Bool False) _ e) = e
    simpExpr (Plus (Const 0) x) = x
    simpExpr (Plus x (Const 0)) = x
    simpExpr (Plus (Const x) (Plus (Const y) e)) =
      Plus (Const (x+y)) e
    simpExpr (Times (Const 0) x) = Const 0
    simpExpr (Times x (Const 0)) = Const 0
    simpExpr (Times (Const 1) x) = x
    simpExpr (Times x (Const 1)) = x
    simpExpr (Times (Const x) (Times (Const y) e)) =
      Times (Const (x*y)) e
    simpExpr (Times (Negate x) y) = Negate (Times x y)
    simpExpr (Times x (Negate y)) = Negate (Times x y)
    simpExpr (Times (Const x) (Plus y z)) = Plus (Times (Const x) y) (Times (Const x) z)
    simpExpr (Times e (Const x)) | not (isConstant e) = Times (Const x) e
      where
        isConstant Const{} = True
        isConstant _ = False
    simpExpr (Negate (Plus x y)) = Plus (Negate x) (Negate y)
    simpExpr (Plus (Negate e) e') | e == e' = Const 0
    simpExpr (Plus (Negate e) (Plus e' e1)) | e == e' = e1
    -- Normalise so that constants come together
    simpExpr (Times (Times x y) z) = Times x (Times y z)
    simpExpr (Times x (Times y z)) = Times x' (Times y' z)
      where
        [x', y'] = sort [x, y]
    simpExpr (Times x y) = Times x' y'
      where
        [x', y'] = sort [x, y]
    -- Normalise so that e and Negate e come together, and constants
    simpExpr (Plus (Plus x y) z) = Plus x (Plus y z)
    simpExpr (Plus x (Plus y z)) = Plus x' (Plus y' z)
      where
        [x', y'] = sortBy (comparing cmp) [x, y]
    simpExpr (Plus x y) = Plus x' y'
      where
        [x', y'] = sortBy (comparing cmp) [x, y]
    simpExpr e = e

    cmp (Negate e) = (e, False)
    cmp e = (e, True)

    constantValue e =
      case filter impure (universeBi e) of
        [] -> Just (eval undefined undefined e)
        _  -> Nothing
      where
        impure Var{} = True
        impure Delta = True
        impure Old{} = True
        impure IntegralReset{} = True
        impure _ = False

lower :: Process -> Process
lower = fixpoint (eliminateState . eliminateCond . eliminateMinMax . eliminateDeriv . simplify)

eliminateState :: Process -> Process
eliminateState p =
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
        let e' = Cond reset 0 (Var x + Delta * e) in
        (continuous x 0 e',
         -- x itself is the *old* value of the integral
         e')
    lowerExpr _ = Nothing

    replace e1 e2 p =
      transformBi (\e -> if e == e1 then e2 else e) p

eliminateCond :: Process -> Process
eliminateCond = fixpoint (both elim)
  where
    elim s =
      case [cond | Cond cond _ _ <- universeBi s] of
        [] -> s
        (cond:_) ->
          If cond
            (transformBi (elimCond cond True) s)
            (transformBi (elimCond cond False) s)

    chooseCond cond True (Cond cond' e _) | cond == cond' = e
    chooseCond cond False (Cond cond' _ e) | cond == cond' = e
    chooseCond _ _ e = e

    elimCond cond True (If cond' s _) | cond == cond' = s
    elimCond cond False (If cond' _ s) | cond == cond' = s
    elimCond cond val s = transformBi (chooseCond cond val) s

eliminateMinMax :: Process -> Process
eliminateMinMax = transformBi f
  where
    f (Min e1 e2) = Cond (e1 >=? e2) e2 e1
    f (Max e1 e2) = Cond (e1 >=? e2) e1 e2
    f e = e

eliminateDeriv :: Process -> Process
eliminateDeriv = transformBi f
  where
    f (Deriv e) = (e - Old e) / Old Delta
    f e = e

definition :: Process -> Var -> (Maybe Expr, Maybe Expr)
definition p x = (definitionS (start p) x, definitionS (step p) x)

definitionS :: Step -> Var -> Maybe Expr
definitionS (If cond s1 s2) x =
  case (definitionS s1 x, definitionS s2 x) of
    (Nothing, Nothing) -> Nothing
    (Just e, Nothing)  -> Just e
    (Nothing, Just e)  -> Just e
    (Just e1, Just e2) -> Just (Cond cond e1 e2)

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

constant :: Value -> Expr
constant (DoubleValue x) = Const x
constant (BoolValue x) = Bool x

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
eval delta env (Min e1 e2) =
  DoubleValue (min (double (eval delta env e1)) (double (eval delta env e2)))
eval delta env (Max e1 e2) =
  DoubleValue (max (double (eval delta env e1)) (double (eval delta env e2)))
eval _ _ (Old _) =
  error "use 'lower' before evaluation"
eval _ _ (IntegralReset _ _) =
  error "use 'lower' before evaluation"
eval _ _ (Deriv _) =
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
  loop Map.empty [] (Map.empty:inputs) (start process':repeat (step process'))
  where
    process' = lower process

    loop _ history [] _ = (reverse history, OK)
    loop env history (inp:inps) (step:steps) =
      case execStep delta (Map.union inp env) step of
        (env, OK) -> loop env (env:history) inps steps
        (env, err) -> (reverse (env:history), err)
