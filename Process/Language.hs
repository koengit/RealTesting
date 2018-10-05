-- Basic data types and miscellaneous functions.
{-# LANGUAGE DeriveDataTypeable #-}
module Process.Language where

import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Generics.Uniplate.Data
import Data.List
import Utils
import Data.Data
import Control.Monad
import Data.Either

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
   If Expr Step Step       -- if-then-else
 | Assume String Expr Step -- check precondition
 | Assert String Expr Step -- check postcondition
 | Update (Map Var Expr)   -- update variables
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
 | Cond Expr Expr Expr -- can be lowered with eliminateCond
   -- Primitive which must be lowered before evaluation
 | Primitive PrimitiveKind String [Expr]
 deriving (Eq, Ord, Typeable, Data)

-- Functional primitives are a function of the current value of their arguments.
-- Temporal primitives take the whole history of the program into account.
data PrimitiveKind = Functional | Temporal
  deriving (Eq, Ord, Typeable, Data)

data Var =
    Global String
  | Local Int
  deriving (Eq, Ord, Typeable, Data)

-- The definition of a primitive
type Prim = [Expr] -> (Expr -> Process) -> Process

instance Num Expr where
  fromInteger = Double . fromInteger
  -- This simplification gets sum to behave reasonably
  Double 0 + x = x
  x + Double 0 = x
  x + y = Plus x y
  Double 1 * x = x
  x * Double 1 = x
  x * y = Times x y
  negate x = Negate x
  abs x = Cond (Positive x) x (negate x)
  signum x = Cond (Zero x) 0 (Cond (Positive x) 1 (-1))

instance Fractional Expr where
  fromRational = Double . fromRational
  recip x = Power x (-1)

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

-- Find all subexpressions.
exprs :: Data a => a -> [Expr]
exprs = universeBi

-- Find all subexpressions, skipping terms under temporal primitives.
functionalExprs :: Data a => a -> [Expr]
functionalExprs = concatMap f . childrenBi
  where
    f e =
      e:
      case e of
        Primitive Temporal _ _ -> []
        _ -> concatMap functionalExprs (children e)

-- Replaces an expression with another everywhere.
-- Does *not* replace under temporal primitives.
replaceLocal :: Data a => Expr -> Expr -> a -> a
replaceLocal e1 e2 = descendBi f
  where
    f e | e == e1 = e2
    f e@(Primitive Temporal _ _) = e
    f e = descend f e

-- Replaces an expression with another everywhere,
-- even under temporal primitives.
replaceGlobal :: Data a => Expr -> Expr -> a -> a
replaceGlobal e1 e2 = transformBi (\e -> if e == e1 then e2 else e)

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

-- Separates a sum into positive and negative parts
terms :: Expr -> ([Expr], [Expr])
terms e
  | k > 0     = (Double k:pos, neg)
  | k < 0     = (pos, Double (-k):neg)
  | otherwise = (pos, neg)
  where
    (k, pos, neg) = terms' e

-- Separates a sum into positive and negative parts and a constant
terms' :: Expr -> (Double, [Expr], [Expr])
terms' e = (k, pos', neg')
  where
    (k, pos, neg) = go e
    (pos', neg') = collectLikeTerms (map factors pos ++ map (factors . Negate) neg)

    go (Plus e1 e2) = (k1 + k2, pos1 ++ pos2, neg1 ++ neg2)
      where
        (k1, pos1, neg1) = go e1
        (k2, pos2, neg2) = go e2
    go (Negate e) = (-k, neg, pos)
      where
        (k, pos, neg) = go e
    go (Double x) = (x, [], [])
    go e = (0, [e], [])

    -- Look for terms which are the same modulo a constant factor,
    -- e.g. 3x^2 and 4x^2, collect them together
    collectLikeTerms =
      partitionEithers .
      map toTerm .
      map addFactors .
      partitionBy (sort . snd)
      where
        addFactors es@((_, fs):_) =
          (sum (map fst es), fs)
        toTerm (k, es)
          | k < 0 = Right (Double (negate k) * product es)
          | otherwise = Left (Double k * product es)

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

-- Separates a product into numerator and denominator parts and a constant.
-- Removes common factors from numerator and denominator.
factors' :: Expr -> (Double, [Expr], [Expr])
factors' e = (k, num \\ denom, denom \\ num)
  where
    (k, num, denom) = go e

    go (Times e1 e2) = (k1 * k2, num1 ++ num2, denom1 ++ denom2)
      where
        (k1, num1, denom1) = go e1
        (k2, num2, denom2) = go e2
    go (Negate e) = (-k, num, denom)
      where
        (k, num, denom) = go e
    go (Power e (Negate x)) = (recip k, denom, num)
      where
        (k, num, denom) = go e
        power e =
          case x of
            Double 1 -> e
            _ -> Power e x
    go (Power e (Double x)) | x < 0 = go (Power e (Negate (Double (-x))))
    go (Double x) = (x, [], [])
    go e = (1, [e], [])

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
