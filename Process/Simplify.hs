-- Simplification.
module Process.Simplify where

import qualified Data.Map.Strict as Map
import Data.Generics.Uniplate.Data
import Utils
import Data.Data
import Process.Language
import Process.Eval
import Data.Either
import Data.List

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
    simpStep (If _ s s') | s == s' = s
    simpStep (If e s1 s2) =
      If e
        (propagateBool e True s1)
        (propagateBool e False s2)
    simpStep (Assume _ (Bool True) s) = s
    simpStep (Assert _ (Bool True) s) = s
    simpStep s = s

simplifyExpr :: Expr -> Expr
simplifyExpr = fixpoint (transformBi simp)
  where
    simp e | Just x <- evalM Nothing Map.empty e = constant x
    simp (Cond (Bool True) e _) = e
    simp (Cond (Bool False) _ e) = e
    simp (Cond _ e e') | e == e' = e
    simp (Cond cond e1 e2) =
      Cond cond (propagateBool cond True e1) (propagateBool cond False e2)
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
    -- Don't descend into temporal operators - it's not sound
    propagate _ _ e@(Primitive Temporal _ _) = e
    propagate cond val e = descend (propagate cond val) e

    -- This is pretty crappy but helps with abs somewhat
    -- (no need for Zero/Zero case because simplify will normalise)
    implies (Zero e1) (Positive e2)
      | (k, [], []) <- terms' (Plus e2 (Negate e1)),
        k >= 0 = True
    implies (Zero e1) (Positive e2)
      | (k, [], []) <- terms' (Plus e2 e1),
        k >= 0 = True
    implies (Positive e1) (Positive e2)
      | (k, [], []) <- terms' (Plus e2 (Negate e1)),
        k >= 0 = True
    implies e1 (And e2 e3) =
      implies e1 e2 && implies e1 e3
    implies (And e1 e2) e3 =
      implies e1 e3 || implies e2 e3
    implies e1 e2 = e1 == e2

-- Eliminate difficult constructs
lower :: [(String, Prim)] -> Process -> Process
lower prims = fixpoint (eliminatePrims prims . eliminateCond . simplify)

eliminatePrims :: [(String, Prim)] -> Process -> Process
eliminatePrims prims =
  fixpoint $ \p ->
    case [ (e, f)
         | e@(Primitive _ name _) <- universeBi p,
           Just f <- [lookup name prims] ] of
      [] -> p
      (e@(Primitive _ _ es), f):_ ->
        f es (\e' -> replaceGlobal e e' p)
      _ -> error "unreachable"

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
      where
        orr x y = Not (And (Not x) (Not y))

    isBool And{} = True
    isBool Not{} = True
    isBool Zero{} = True
    isBool Positive{} = True
    isBool _ = False

    findConds e = [cond | Cond cond _ _ <- functionalExprs e]

-- Expand out multiplication.
expand :: Expr -> Expr
expand = fixpoint (transformBi expand1)
  where
    expand1 (Times x (Plus y z)) = Plus (Times x y) (Times x z)
    expand1 (Times (Plus x y) z) = Plus (Times x z) (Times y z)
    expand1 (Power e 0) = 1
    expand1 (Power e (Double k)) | k >= 1 = e * Power e (Double (k-1))
    expand1 x = x

-- Given a variable x and an expression e (which should contain x),
-- solve the equation e=0 for x. A pretty lousy implementation.
solve :: Expr -> Expr -> Expr
solve x e
  | x `elem` functionalExprs result = error "couldn't solve"
  | otherwise = result
  -- x*xs + rest = 0
  -- => x = -rest/xs
  where
    result = negate (sum rest) / sum xs
    (pos, neg) = terms (expand e)
    (xs, rest) = partitionEithers (map classify (pos ++ map negate neg))
    classify e
      | x `elem` es = Left (Double k * product (es \\ [x]))
      | otherwise = Right e
      where
        (k, es) = factors e
