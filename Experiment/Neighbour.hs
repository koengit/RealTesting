module Neighbour where

import Data.Ord
import Data.List

import VBool

class HasNeighbours a where
  -- Consider `a -> Gen a`
  neighbours :: a -> [a]

instance HasNeighbours Int where
  neighbours x = [x+1, x-1]

instance HasNeighbours a => HasNeighbours [a] where
  neighbours xs = triangular (zipWith (:) xs $ map neighbours xs)

triangular :: [[a]] -> [[a]]
triangular []       = [[]]
triangular (xs:xss) =
  let rest = triangular xss in
    concat $ [ map (x:) rest | x <- xs ]

hillClimb :: (HasNeighbours a, Ord m) => Int -> (a -> Bool) -> a -> (a -> m) -> a
hillClimb 0 stopping a meas = a
hillClimb n stopping a meas
  | stopping a          = a
  | null (neighbours a) = a
  | otherwise          =
    let best = maximumBy (comparing meas) (neighbours a) in
      if meas best > meas a then
        hillClimb (n - 1) stopping best meas
      else
        a

satisfy :: HasNeighbours a => (a -> VBool) -> a -> Maybe a
satisfy prop a =
  let best = hillClimb 1000 (toBool . prop) a (unVBool . prop) in
   if toBool (prop best) then
    Just best
   else
    Nothing

--------------------------------

isPalindrome :: [Int] -> VBool
isPalindrome xs = xs ==% reverse xs

isSorted :: [Int] -> VBool
isSorted xs = andP $ zipWith (<=%) xs (tail xs)
