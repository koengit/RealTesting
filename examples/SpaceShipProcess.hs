import Process
import qualified Data.Map.Strict as Map

acceleration :: Var
acceleration = Global "acceleration"

position :: Var
position = Global "position"

ship :: Process
ship = continuous position 0 (integral (integral (var acceleration)))

check :: Process
check =
  name $ \state ->
    initially (set state (double 0) & set ok (bool False)) &
    loop (
      ite (var state ==? 0 &&& var position >=?  100)
        (set state 1)
        (ite (var state ==? 1 &&& var position <=? -100) (set state 2)
          (assert (var position <=? 100))))

test :: Valued f => [Double] -> f ([Env], Result)
test vals = simulate 1 envs (ship & check)
  where
    envs = [Map.singleton acceleration (DoubleValue x) | x <- vals]
