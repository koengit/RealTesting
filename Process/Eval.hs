-- An evaluator. Can be used with Val.
{-# LANGUAGE DefaultSignatures, TupleSections, FlexibleInstances #-}
module Process.Eval where

import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Functor.Identity
import Process.Language
import Process.Pretty()
import Text.PrettyPrint.HughesPJClass
import VBool

type Env = Map Var Value
data Value = DoubleValue Double | BoolValue Bool deriving (Eq, Ord)

instance Show Value where
  show (DoubleValue x) = show x
  show (BoolValue x) = show x

data VValue = DoubleVValue Double | VBoolVValue VBool deriving (Eq, Ord)

instance Show VValue where
  show (DoubleVValue x) = show x
  show (VBoolVValue x) = show x

class Valued f where
  -- Very much like Monad, but with if-then-else and Ord constraints
  val :: a -> f a
  vmap :: Ord b => (a -> b) -> f a -> f b
  vlift :: Ord c => (a -> b -> c) -> f a -> f b -> f c
  vbind :: Ord b => f a -> (a -> f b) -> f b
  vifThenElse :: Ord a => VBool -> f a -> f a -> f a
  vprune :: (Ord a, Ord b) => (a -> b) -> f a -> f a
  vprune _ = id
  vbool :: VBool -> f Bool
  vbool = val . isTrue

  -- A default instance for monads.
  default val :: Monad f => a -> f a
  val = return
  default vmap :: (Monad f, Ord b) => (a -> b) -> f a -> f b
  vmap = fmap
  default vlift :: (Monad f, Ord c) => (a -> b -> c) -> f a -> f b -> f c
  vlift = liftM2
  default vbind :: (Monad f, Ord b) => f a -> (a -> f b) -> f b
  vbind = (>>=)
  default vifThenElse :: Monad f => VBool -> f a -> f a -> f a
  vifThenElse x my mz = if isTrue x then my else mz

vsequence :: (Valued f, Ord a) => [f a] -> f [a]
vsequence [] = val []
vsequence (x:xs) = vlift (:) x (vsequence xs)

instance Valued Identity

doubleVal :: VValue -> Double
doubleVal (DoubleVValue x) = x
doubleVal _ = error "type error: got bool but expected double"

boolVal :: VValue -> VBool
boolVal (VBoolVValue x) = x
boolVal _ = error "type error: got double but expected bool"

constant :: VValue -> Expr
constant (DoubleVValue x) = Double x
constant (VBoolVValue x) = Bool (isTrue x)

eval :: Maybe Double -> Env -> Expr -> VValue
eval mdelta env x = runIdentity (evalM mdelta env x)

evalM :: Monad m => Maybe Double -> Env -> Expr -> m VValue
evalM _ env (Var x) =
  case Map.lookup x env of
    Nothing -> fail ("variable " ++ show x ++ " not bound")
    Just (DoubleValue x) -> return (DoubleVValue x)
    Just (BoolValue x) -> return (VBoolVValue (bool x))
evalM mdelta _ Delta =
  case mdelta of
    Nothing -> fail "delta not defined"
    Just delta -> return (DoubleVValue delta)
evalM _ _ (Double x) =
  return (DoubleVValue x)
evalM delta env (Plus e1 e2) =
  DoubleVValue <$> liftM2 (+)
    (doubleVal <$> evalM delta env e1)
    (doubleVal <$> evalM delta env e2)
evalM delta env (Times e1 e2) =
  DoubleVValue <$> liftM2 (*)
    (doubleVal <$> evalM delta env e1)
    (doubleVal <$> evalM delta env e2)
evalM delta env (Power e1 e2) =
  DoubleVValue <$> liftM2 (**)
    (doubleVal <$> evalM delta env e1)
    (doubleVal <$> evalM delta env e2)
evalM delta env (Negate e) =
  DoubleVValue . negate . doubleVal <$> evalM delta env e
evalM delta env (Not e) =
  VBoolVValue . nt . boolVal <$> evalM delta env e
evalM delta env (And e1 e2) =
  VBoolVValue <$> liftM2 (&&+)
    (boolVal <$> evalM delta env e1)
    (boolVal <$> evalM delta env e2)
evalM _ _ (Bool x) =
  return (VBoolVValue (bool x))
evalM delta env (Positive e) =
  VBoolVValue . (>=% 0) . doubleVal <$> evalM delta env e
evalM delta env (Zero e) =
  VBoolVValue . (==% 0) . doubleVal <$> evalM delta env e
evalM _ _ e =
  fail $ show $
    sep [
      text "don't know how to evaluate",
      nest 2 (pPrint e),
      text "(try using 'lower stdPrims' before simulating)"]

data Result = OK | PreconditionFailed String | PostconditionFailed String
  deriving (Eq, Ord, Show)

execStep :: Valued f => Double -> Env -> Step -> f (Env, Result)
execStep delta env (If e s1 s2) =
  vifThenElse (boolVal $ eval (Just delta) env e)
    (execStep delta env s1)
    (execStep delta env s2)
execStep delta env (Assume str e s) =
  vifThenElse (boolVal $ eval (Just delta) env e)
    (execStep delta env s)
    (val (env, PreconditionFailed str))
execStep delta env (Assert str e s) =
  vifThenElse (boolVal $ eval (Just delta) env e)
    (execStep delta env s)
    (val (env, PostconditionFailed str))
execStep delta env (Update m) =
  -- N.B. Map.union is left-biased
  -- Non-valued version: val (Map.union (Map.map (eval (Just delta) env) m) env, OK)
  vmap (\xs -> (Map.union (Map.fromList xs) env, OK)) $
    vsequence [ single x (eval (Just delta) env y) | (x, y) <- Map.toList m ]
  where
    single x (DoubleVValue y) = val (x, DoubleValue y)
    single x (VBoolVValue vb) = vmap (x,) (vmap BoolValue (vbool vb))

simulate :: Valued f => Double -> [Env] -> Process -> f ([Env], Result)
simulate delta inputs process =
  vmap (\(_, history, err) -> (tail (reverse history), err)) $
  foldl sim (val (Map.empty, [], OK)) (zip (Map.empty:inputs) (start process:repeat (step process)))
  where
    sim state (input, step) =
      vprune badness $ vbind state $ \(env, history, err) ->
        case err of
          OK ->
            vmap (\(env, res) -> (env, env:history, res)) $
              execStep delta (Map.union input env) step
          _ -> val (env, history, err)
    badness (_, _, PreconditionFailed _) = 0
    badness (_, _, OK) = 1
    badness (_, _, PostconditionFailed _) = 2
