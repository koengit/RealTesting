{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternGuards, DeriveDataTypeable, DefaultSignatures, TupleSections, StandaloneDeriving, UndecidableInstances #-}
module Process.Eval where

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
import Process.Language
import Process.Pretty

type Env = Map Var Value
data Value = DoubleValue Double | BoolValue Bool deriving (Eq, Ord, Show)

class Valued f where
  -- Very much like Monad, but with if-then-else and Ord constraints
  val :: a -> f a
  vmap :: Ord b => (a -> b) -> f a -> f b
  vlift :: Ord c => (a -> b -> c) -> f a -> f b -> f c
  vfail :: String -> f a
  vbind :: Ord b => f a -> (a -> f b) -> f b
  vifThenElse :: Ord a => f Bool -> f a -> f a -> f a
  vprune :: Ord a => f a -> f a
  vprune = id
  veq :: f Double -> f Double -> f Bool
  vgeq :: f Double -> f Double -> f Bool

  -- A default instance for monads.
  default val :: Monad f => a -> f a
  val = return
  default vmap :: (Monad f, Ord b) => (a -> b) -> f a -> f b
  vmap = fmap
  default vlift :: (Monad f, Ord c) => (a -> b -> c) -> f a -> f b -> f c
  vlift = liftM2
  default vbind :: (Monad f, Ord b) => f a -> (a -> f b) -> f b
  vbind = (>>=)
  default vfail :: Monad f => String -> f a
  vfail = fail
  default vifThenElse :: Monad f => f Bool -> f a -> f a -> f a
  vifThenElse mx my mz = do
    x <- mx
    if x then my else mz
  default veq :: f Double -> f Double -> f Bool
  veq = vlift (==)
  default vgeq :: f Double -> f Double -> f Bool
  vgeq = vlift (>=)

vsequence :: (Valued f, Ord a) => [f a] -> f [a]
vsequence [] = val []
vsequence (x:xs) = vlift (:) x (vsequence xs)

instance Valued Identity
instance Valued Maybe
instance Valued (Either String)

doubleVal :: Value -> Double
doubleVal (DoubleValue x) = x
doubleVal _ = error "type error: got bool but expected double"

boolVal :: Value -> Bool
boolVal (BoolValue x) = x
boolVal _ = error "type error: got double but expected bool"

constant :: Value -> Expr
constant (DoubleValue x) = Double x
constant (BoolValue x) = Bool x

eval :: Valued f => Maybe Double -> Env -> Expr -> f Value
eval _ env (Var x) =
  case Map.lookup x env of
    Nothing -> vfail ("variable " ++ show x ++ " not bound")
    Just v -> val v
eval mdelta _ Delta =
  case mdelta of
    Nothing -> vfail "delta not defined"
    Just delta -> val (DoubleValue delta)
eval _ _ (Double x) =
  val (DoubleValue x)
eval delta env (Plus e1 e2) =
  vmap DoubleValue $ vlift (+)
    (vmap doubleVal $ eval delta env e1)
    (vmap doubleVal $ eval delta env e2)
eval delta env (Times e1 e2) =
  vmap DoubleValue $ vlift (*)
    (vmap doubleVal $ eval delta env e1)
    (vmap doubleVal $ eval delta env e2)
eval delta env (Power e1 e2) =
  vmap DoubleValue $ vlift (**)
    (vmap doubleVal $ eval delta env e1)
    (vmap doubleVal $ eval delta env e2)
eval delta env (Negate e) =
  vmap (DoubleValue . negate . doubleVal) (eval delta env e)
eval delta env (Not e) =
  vmap (BoolValue . not . boolVal) (eval delta env e)
eval delta env (And e1 e2) =
  vmap BoolValue $ vlift (&&)
    (vmap boolVal $ eval delta env e1)
    (vmap boolVal $ eval delta env e2)
eval _ _ (Bool x) =
  val (BoolValue x)
eval delta env (Positive e) =
  vmap BoolValue . flip vgeq (val 0) . vmap doubleVal $ (eval delta env e)
eval delta env (Zero e) =
  vmap BoolValue . veq (val 0) . vmap doubleVal $ (eval delta env e)
eval delta env (Cond e1 e2 e3) =
  vifThenElse
    (vmap boolVal $ eval delta env e1)
    (eval delta env e2)
    (eval delta env e3)
eval _ _ e =
  vfail ("dont't know how to evaluate " ++ show e)

data Result = OK | PreconditionFailed Expr | PostconditionFailed Expr
  deriving (Eq, Ord, Show)

execStep :: Valued f => Double -> Env -> Step -> f (Env, Result)
execStep delta env (If e s1 s2) =
  vifThenElse (vmap boolVal $ eval (Just delta) env e)
    (execStep delta env s1)
    (execStep delta env s2)
execStep delta env (Assume e s) =
  vifThenElse (vmap boolVal $ eval (Just delta) env e)
    (execStep delta env s)
    (val (env, PreconditionFailed e))
execStep delta env (Assert e s) =
  vifThenElse (vmap boolVal $ eval (Just delta) env e)
    (execStep delta env s)
    (val (env, PostconditionFailed e))
execStep delta env (Update m) =
  -- N.B. Map.union is left-biased
  -- Non-valued version: val (Map.union (Map.map (eval (Just delta) env) m) env, OK)
  vmap (\xs -> (Map.union (Map.fromList xs) env, OK)) $
    vsequence [ vmap (x,) (eval (Just delta) env y) | (x, y) <- Map.toList m ]

simulate :: Valued f => Double -> [Env] -> Process -> f ([Env], Result)
simulate delta inputs process =
  vmap (\(_, history, err) -> (reverse history, err)) $
  foldl sim (val (Map.empty, [], OK)) (zip (Map.empty:inputs) (start process:repeat (step process)))
  where
    sim state (input, step) =
      vprune $ vbind state $ \(env, history, err) ->
        case err of
          OK ->
            vmap (\(env, res) -> (env, env:history, res)) $
              execStep delta (Map.union input env) step
          _ -> val (env, history, err)
