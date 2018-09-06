{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternGuards, DeriveDataTypeable, DefaultSignatures, TupleSections #-}
module Process.Simplify where

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
import Process.Core
import Process.Eval

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
    simp e | Just x <- eval Nothing Map.empty e = constant x
    simp (Ite (Bool True) e _) = e
    simp (Ite (Bool False) _ e) = e
    simp (Ite _ e e') | e == e' = e
    simp (Ite cond e1 e2) =
      Ite cond (propagateBool cond True e1) (propagateBool cond False e2)
    simp (Not (Not e)) = e
    simp (Times (Double x) (Plus y z)) = Plus (Times (Double x) y) (Times (Double x) z)
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
        ([], []) -> Double 0
        (pos, neg) ->
          foldr1 Plus (pos ++ map Negate neg)
    simp e@Times{} =
      case factors e of
        (0, _) -> Double 0
        (k, []) -> Double k
        (k, es) ->
          -- If k is negative, multiply with abs k and use Negate
          -- (this simplifies better if inside a Plus)
          (if k < 0 then Negate else id) $
          -- Multiply with abs k only if necessary
          (if abs k == 1 then id else Times (Double (abs k))) $
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
type Prim = [Expr] -> Named Expr

lower :: [(String, Prim)] -> Process -> Process
lower prims = fixpoint (eliminatePrims prims . eliminateIte . eliminateMinMax . simplify)

eliminatePrims :: [(String, Prim)] -> Process -> Process
eliminatePrims prims =
  transformExprInProcess lower
  where
    lower (Primitive name es) =
      case lookup name prims of
        Nothing -> error ("unknown primitive " ++ name)
        Just f -> f es
    lower e = return e

eliminateIte :: Process -> Process
eliminateIte =
  fixpoint (both (transformBi f . transformBi introBool . simplifyStep))
  where
    -- Idea: replace
    --   ... Ite cond e1 e2 ...
    -- with
    --   if cond then [... Ite cond e1 e2 ...] else [... Ite cond e1 e2 ...]
    -- and then use propagateBool to get rid of the Ite in each branch
    f s =
      case findItes s of
        [] -> s
        cond:_ ->
          If cond
            (propagateBool cond True s)
            (propagateBool cond False s)
    -- If Ite is the argument to a predicate (And, Not, Zero, Positive),
    -- encode it using Boolean connectives:
    --   P(Ite e1 e2 e3) = (e1 && P(e2)) || (not e1 && P(e3))
    -- Actually, we produce:
    --   (e1 && P(Ite e1 e2 e3)) || (not e1 && P(Ite e1 e2 e3))
    -- and leave it to propagateBool to simplify.
    introBool e
      | isBool e, cond:_ <- findItes e =
         orr
           (And cond (propagateBool cond True e))
           (And (Not cond) (propagateBool cond False e))
      | otherwise = e
      where
        orr x y = Not (And (Not x) (Not y))

    isBool And{} = True
    isBool Not{} = True
    isBool Zero{} = True
    isBool Positive{} = True
    isBool _ = False

    findItes e = [cond | Ite cond _ _ <- exprs e]

eliminateMinMax :: Process -> Process
eliminateMinMax = transformBi f
  where
    f (Min e1 e2) = Ite (Positive (e1 - e2)) e2 e1
    f (Max e1 e2) = Ite (Positive (e1 - e2)) e1 e2
    f e = e
