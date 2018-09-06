module Process(
  module Process,
  module Process.Language,
  module Process.Combinators,
  module Process.Eval,
  module Process.Simplify,
  module Process.Pretty) where

import Process.Language(Process, Step, Expr, PrimitiveKind(..), Var(..))
import Process.Combinators
import Process.Eval(Value(..), Env, Valued(..), Result(..))
import qualified Process.Eval
import Process.Simplify(lower, simplify)
import Process.Pretty()

simulate :: Valued f => Double -> [Env] -> Process -> f ([Env], Result)
simulate delta input p =
  Process.Eval.simulate delta input (lower stdPrims p)
