module Shape where

import Test.QuickCheck

type Tag   = Int
data Shape = Nary Int | Nested Tag [Shape] deriving (Show, Eq)

instance Arbitrary Shape where
  arbitrary = sized gen
    where
      gen 0 = Nary <$> arbitrary
      gen n = oneof [ Nested <$> arbitrary <*> (listOf (gen (n `div` 5)))
                    , gen 0]

  shrink (Nary n) = Nary <$> shrink n

  shrink (Nested n xs) = (Nary <$> shrink n) ++ [ Nested n x |
                                                    x <- map shrink xs]

flatten :: Shape -> Int
flatten (Nary n) = n
flatten (Nested _ ss) = sum (flatten <$> ss)

splitBy :: [Int] -> [Double] -> [[Double]]
splitBy [] []     = []
splitBy (x:xs) ds = (take x ds) : (splitBy xs (drop x ds))
splitBy _ _       = error "Shape mismatch"

reconstruct :: Shape -> [Double] -> [[Double]]
reconstruct (Nary n) ds
  | n == length ds = [ [d] | d <- ds ]
  | otherwise      = error "Shape mismatch"
reconstruct (Nested _ ss) ds = splitBy (map flatten ss) ds

class HasShape a where
  shapeOf   :: a -> Shape
  fromRn    :: Shape -> [Double] -> a
  measure   :: a -> [Double]

{- Laws:
  fromRn (shapeOf a) (measure a) == a
-}

instance HasShape Double where
  shapeOf _  = Nary 1

  fromRn _   = head

  measure x  = [x]

instance HasShape Int where
  shapeOf _  = Nary 1

  fromRn _   = round . head

  measure x  = [fromInteger (toInteger x)]

instance HasShape a => HasShape [a] where
  shapeOf xs            = Nested 0 $ shapeOf <$> xs

  fromRn (Nested 0 ss) ds =
    [ fromRn s d | (s, d) <- zip ss (reconstruct (Nested 0 ss) ds) ]

  measure xs            = concat $ measure <$> xs

instance (HasShape a, HasShape b) => HasShape (a, b) where
  shapeOf (a, b) = Nested 0 [shapeOf a, shapeOf b]

  fromRn (Nested 0 [ashape, bshape]) ds =
    (fromRn ashape (take (flatten ashape) ds),
     fromRn bshape (drop (flatten ashape) ds))

  measure (a, b) = measure a ++ measure b

prop_law :: (Eq a, HasShape a) => a -> Bool
prop_law xs = fromRn (shapeOf xs) (measure xs) == xs
