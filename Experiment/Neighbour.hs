import Data.Ord
import Data.List

import VBool

class HasNeighbours a where
  -- Consider `a -> Gen a`
  neighbours :: a -> [a]

instance HasNeighbours Int where
  neighbours x = [x+1, x-1]

instance HasNeighbours a => HasNeighbours [a] where
  neighbours xs = undefined

hillClimb :: (HasNeighbours a, Ord m) => Int -> a -> (a -> m) -> a
hillClimb 0 a meas = a
hillClimb n a meas
  | null (neighbours a) = a
  | otherwise          =
    let best = maximumBy (comparing meas) (neighbours a) in
      if meas best >= meas a then
        hillClimb (n - 1) best meas
      else
        a

satisfy :: HasNeighbours a => (a -> VBool) -> a -> Maybe a
satisfy prop a =
  let best = hillClimb 100000 a (unVBool . prop) in
   if toBool (prop best) then
    Just best
   else
    Nothing

--------------------------------

prop_palindrome :: [Int] -> VBool
prop_palindrome xs = xs ==% reverse xs
