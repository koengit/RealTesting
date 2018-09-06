{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternGuards, DeriveDataTypeable, DefaultSignatures, TupleSections #-}
module Process.Core where

import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Generics.Uniplate.Data
import Data.Generics.Str(strStructure)
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
import Control.Arrow((***))
import Data.Functor.Identity

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
   If Expr Step Step         -- if-then-else
 | Assume Expr Step        -- check precondition
 | Assert Expr Step        -- check postcondition
 | Update (Map Var Expr) -- update variables
 deriving (Eq, Typeable, Data)

data Expr =
   Var Var      -- variable
 | Delta        -- delta-t
   -- Arithmetic
 | Double Double -- constant
 | Plus Expr Expr
 | Times Expr Expr
 | Power Expr Expr
 | Negate Expr
   -- Booleans
 | Not Expr
 | And Expr Expr
 | Bool Bool
 | Positive Expr -- e >= 0
 | Zero Expr     -- e == 0
   -- All these operations can be eliminated by calling 'lower'
 | Ite Expr Expr Expr
 | Min Expr Expr
 | Max Expr Expr
 | Primitive String [Expr] -- primitive which must be lowered
 deriving (Eq, Ord, Typeable, Data)

data Var =
    Global String
  | Local Int
  deriving (Eq, Ord, Typeable, Data)

----------------------------------------------------------------------
-- Combinators for building processes.
----------------------------------------------------------------------

instance Num Expr where
  fromInteger = Double . fromInteger
  x + y = Plus x y
  x * y = Times x y
  negate x = Negate x
  abs x = Ite (x >=? 0) x (negate x)
  signum x = Ite (x ==? 0) 0 (Ite (x >=? 0) 1 (-1))

instance Fractional Expr where
  fromRational = Double . fromRational
  recip x = Power x (-1)

orr :: Expr -> Expr -> Expr
orr e1 e2 = Not (Not e1 `And` Not e2)

infix 4 ==?, >=?, <=?, /=?
(==?), (>=?), (<=?) :: Expr -> Expr -> Expr
x ==? y = Zero (x-y)
x /=? y = Not (x ==? y)
x >=? y = Positive (x-y)
x <=? y = y >=? x

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

-- Generate a local name
name :: (Var -> Process) -> Process
name f = fst (name_ (\x -> (f x, ())))

name_ :: (Var -> (Process, a)) -> (Process, a)
name_ f = (p{locals = locals p + 1}, x)
  where
    (p, x) = f xs
    xs = Local (locals p)

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

exprs :: Data a => a -> [Expr]
exprs = universeBi

replace :: Data a => Expr -> Expr -> a -> a
replace e1 e2 = transformBi (\e -> if e == e1 then e2 else e)

data Named a =
    Return a
  | Name (Var -> Named (Process, a))

instance Functor Named where
  fmap = liftM
instance Applicative Named where
  pure = return
  (<*>) = ap
instance Monad Named where
  return = Return
  Return x >>= k = k x
  Name f >>= k =
    Name $ \x -> do
      (p, y) <- f x
      z <- k y
      return (p, z)

instance Par a => Par (Named a) where
  skip = Return skip
  Return x & Return y =
    Return (x & y)
  mx & Name f = do
    x <- mx
    Name $ \y -> do
      (p, z) <- f y
      return (p, x & z)
  x@Name{} & y@Return{} = y & x

compileNamed_ :: Named a -> (Process, a)
compileNamed_ (Return e) = (skip, e)
compileNamed_ (Name f) =
  name_ $ \x ->
    -- XXX do duplicate elimination here
    let (p, (q, y)) = compileNamed_ (f x)
    in (p & q, y)

compileNamed :: Named a -> (a -> Process) -> Process
compileNamed x k = k y & p
  where
    (p, y) = compileNamed_ x

transformExprInProcess :: (Expr -> Named Expr) -> Process -> Process
transformExprInProcess f p = compileNamed (transformBiM f p) id

instance Pretty a => Show (Named a) where
  show = show . pPrint
instance Pretty a => Pretty (Named a) where
  pPrint (Return x) = pPrint x
  pPrint e =
    parens $
      let (p, x) = compileNamed_ e in
      sep [pPrint x, text "with" <+> pPrint p]

instance Show Process where
  show = show . pPrint

instance Show Step where
  show = show . pPrint

instance Show Expr where
  show = show . pPrint

instance Show Var where
  show = show . pPrint

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
ppExp _ (Double x) = text (shortest (show x) (printf "%.5f" x))
  where
    shortest x y
      | length x <= length y = x
      | otherwise = y
ppExp n (Plus e1 e2) =
  maybeParens (n > 4) $
    fsep $
      (case e1 of Negate{} -> ppTerm "-" e1; _ -> ppExp 4 e1):
      map (ppTerm "+") pos ++
      map (ppTerm "-") neg
  where
    (pos, neg) = terms e2
    ppTerm s e = text s <+> ppExp 4 e
ppExp n e@Times{} =
  maybeParens (n > 6) $
  fsep $ punctuate (text " *") $
  map (ppExp 7) $
    [Double k | k /= 1] ++ es
  where
    (k, es) = factors e
ppExp n (Power e (Double (-1))) =
  ppUnary n "1/" 7 e
ppExp n (Power e1 e2) =
  maybeParens (n > 7) $
    cat [ppExp 8 e1 <#> text "^", nest 2 (ppExp 8 e2)]
ppExp n (Negate e) = ppUnary n "-" 5 e
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
ppExp n (Ite e1 e2 e3) =
  maybeParens (n > 0) $
    -- else-branch must be atomic to avoid ambiguity
    ppIfThenElse (ppExp 0 e1) (ppExp 0 e2) (ppExp 9 e3)
ppExp n (Min e1 e2) =
  ppFunction "min" [e1, e2]
ppExp n (Max e1 e2) =
  ppFunction "max" [e1, e2]
ppExp n (Primitive name es) =
  ppFunction name es

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
ppSum [] = Double 0
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

-- Separates a sum into positive and negative parts
terms :: Expr -> ([Expr], [Expr])
terms e
  | k > 0  = (Double k:pos, neg)
  | k < 0  = (pos, Double (-k):neg)
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
    go (Double x) = (x, [], [])
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
factors (Double x) = (x, [])
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
