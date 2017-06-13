module Shape where

import Test.QuickCheck

data Shape = Nary Int | Nested [Shape] deriving (Show, Eq)

instance Arbitrary Shape where
  arbitrary = sized gen
    where
      gen 0 = Nary <$> arbitrary
      gen n = oneof [ Nested <$> (listOf (gen (n `div` 5)))
                    , gen 0]

ndim :: Int -> Shape
ndim = Nary

n_by_m :: Int -> Int -> Shape
n_by_m n m = Nested [ndim m | _ <- [1..n]]

flatten :: Shape -> Int
flatten (Nary n) = n
flatten (Nested ss) = sum (flatten <$> ss)

splitBy :: [Int] -> [Double] -> [[Double]]
splitBy [] []     = []
splitBy (x:xs) ds = (take x ds) : (splitBy xs (drop x ds))
splitBy _ _       = error "Shape mismatch"

reconstruct :: Shape -> [Double] -> [[Double]]
reconstruct (Nary n) ds
  | n == length ds = [ [d] | d <- ds ]
  | otherwise      = error "Shape mismatch"
reconstruct (Nested ss) ds = splitBy (map flatten ss) ds

class HasShape a where
  shapeOf   :: a -> Shape
  fromRn    :: Shape -> [Double] -> a
  measure   :: a -> [Double] 

{- Laws:
  fromRn (shapeOf a) (measure a) == a
-}

instance HasShape Double where
  shapeOf _  = ndim 1

  fromRn _   = head

  measure x  = [x]

instance HasShape Int where
  shapeOf _  = ndim 1

  fromRn _   = round . head

  measure x  = [fromInteger (toInteger x)]

instance HasShape a => HasShape [a] where
  shapeOf xs            = Nested $ shapeOf <$> xs

  fromRn (Nested ss) ds =
    [ fromRn s d | (s, d) <- zip ss (reconstruct (Nested ss) ds) ]

  measure xs            = concat $ measure <$> xs

instance (HasShape a, HasShape b) => HasShape (a, b) where
  shapeOf (a, b) = Nested [shapeOf a, shapeOf b]

  fromRn (Nested [ashape, bshape]) ds =
    (fromRn ashape (take (flatten ashape) ds),
     fromRn bshape (drop (flatten ashape) ds))

  measure (a, b) = measure a ++ measure b

instance HasShape Shape where
  shapeOf (Nested xs) = Nested (map shapeOf xs)
  shapeOf (Nary n)    = Nary 1

  fromRn (Nested xs) ds  = Nested (fromRn (shapeOf xs) ds)
  fromRn (Nary 1)    [d] = Nary (round d)

  measure (Nested xs) = concat (map measure xs)
  measure (Nary n)    = [fromInteger (toInteger n)]

prop_law :: (Eq a, HasShape a) => a -> Bool
prop_law xs = fromRn (shapeOf xs) (measure xs) == xs
