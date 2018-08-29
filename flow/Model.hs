module Model where

import Data.Set(Set)
import Data.Map(Map)

type Var = String
type ModeName = String

-- A whole model
data Model =
  Model {
    -- The state variables
    vars :: Set Var,
    -- The modes
    modes :: Map ModeName Mode,
    -- The initial mode
    initialMode :: ModeName,
    -- Constraints on the initial values.
    initialVars :: [Constraint] }
  deriving Show

-- A single mode
data Mode =
  Mode {
    -- Values of the derivatives in terms of the current values
    derivatives :: Map Var Expr,
    -- A constraint which must be satisfied for a continuous step to
    -- be possible
    invariant :: [Constraint],
    -- Discrete state changes
    jumps :: [Jump],
    -- States which must not be entered. Conjunction of constraints
    unsafe :: [Constraint] }
  deriving Show

-- A single jump
data Jump =
  Jump {
    -- The mode to jump to
    target :: ModeName,
    -- Constraints which must be satisfied in order to make the jump
    condition :: [Constraint],
    -- The new values of the *derivatives* of the variables.
    -- This map need not contain every variable
    reset :: Map Var Expr }
  deriving Show

-- A constraint
data Constraint =
    -- The expression must be zero
    Zero Expr
    -- The expression must be non-negative
  | Positive Expr
    -- The expression must lie in a given interval
  | Expr `In` (Double, Double)
  deriving Show

-- An expression
data Expr =
    Var Var
  | Const Double
  | Plus Expr Expr
  | Times Expr Expr
  | Power Expr Expr
  | Negate Expr
  | Sin Expr
    -- Has a nondeterministic value which lies in a certain interval.
    -- I think this can only appear as a top-level expression of the
    -- form e + [x..y]?
  | Interval Double Double
  deriving Show
