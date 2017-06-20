module Neighbour where

import Data.Ord
import Data.List
import Test.QuickCheck

import VBool

class HasNeighbours a where
  -- Consider `a -> Gen a`
  neighbours :: a -> [a]

instance HasNeighbours Int where
  neighbours x = [x+1, x-1]

instance HasNeighbours a => HasNeighbours [a] where
  neighbours xs = 
    (if not (null xs) then
      [tail xs, init xs, xs ++ tail xs, tail xs ++ xs, xs ++ xs]
    else
      []) ++ triangular (zipWith (:) xs $ map neighbours xs)

instance
  (HasNeighbours a, HasNeighbours b, HasNeighbours c) =>
  HasNeighbours (a, b, c) where
  neighbours (a, b, c) = [ (a', b', c')
                         | a' <- neighbours a
                         , b' <- neighbours b
                         , c' <- neighbours c]

instance (HasNeighbours a, HasNeighbours b) =>
  HasNeighbours (a, b) where
  neighbours (a, b) = [ (a', b')
                         | a' <- neighbours a
                         , b' <- neighbours b]

triangular :: [[a]] -> [[a]]
triangular []       = [[]]
triangular (xs:xss) =
  let rest = triangular xss in
    concat $ [ map (x:) rest | x <- xs ]

hillClimb :: (HasNeighbours a, Ord m)
          => Int -> (a -> Bool) -> a -> (a -> m) -> Gen a
hillClimb 0 stopping a meas = return a
hillClimb n stopping a meas
  | stopping a          = return a
  | null (neighbours a) = return a
  | otherwise          = do
      b <- elements (neighbours a)
      if meas b > meas a then
        hillClimb (n - 1) stopping b meas
      else
        hillClimb (n - 1) stopping a meas

satisfy :: HasNeighbours a => (a -> VBool) -> a -> IO (Maybe a)
satisfy prop a = do
  let best = hillClimb 100000 (toBool . prop) a (unVBool . prop)
  b <- generate best
  if toBool (prop b) then
   return (Just b)
  else
   return Nothing

--------------------------------

isPalindrome :: [Int] -> VBool
isPalindrome xs = xs ==% reverse xs

isSorted :: [Int] -> VBool
isSorted xs = andP $ zipWith (<=%) xs (tail xs)
