import Falsify

import Test.QuickCheck
import Data.Maybe

prop_palindrome :: [Int] -> VBool
prop_palindrome xs = xs ==% reverse xs

strictlyAscending :: [Int] -> VBool
strictlyAscending xs = andP $ zipWith (<%) xs (tail xs)

isSorted :: [Int] -> VBool
isSorted xs = andP $ zipWith (<=%) xs (tail xs)

example_palindrome :: [Int] -> Maybe [Int]
example_palindrome xs = satisfy xs 3000 100000 prop_palindrome

example_ascending :: Maybe [Int]
example_ascending = satisfy (reverse ([5 .. 16] ++ [1 .. 7]))
                      10000 10000 strictlyAscending

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

data BTree a = Leaf a
             | Node (BTree a) (BTree a)
             deriving (Show, Eq, Ord)

instance HasShape a => HasShape (BTree a) where
  shapeOf (Leaf x)   = Nested 0 [shapeOf x]
  shapeOf (Node x y) = Nested 1 [shapeOf x, shapeOf y]

  fromRn (Nested 0 [s]) ds    = Leaf (fromRn s ds)
  fromRn (Nested 1 [l, r]) ds = Node (fromRn l (take (flatten l) ds))
                                   (fromRn r (drop (flatten l) ds))

  measure (Leaf x)   = measure x
  measure (Node l r) = measure l ++ measure r

instance Arbitrary a => Arbitrary (BTree a) where
  arbitrary = sized gen
    where gen n = case n of
                    0 -> Leaf <$> arbitrary
                    n -> oneof [ gen 0
                               , Node <$> gen (n `div` 2) <*> gen (n `div` 2)
                               ]

prop_isBST :: BTree Int -> VBool
prop_isBST = isSorted . toList

toList :: BTree a -> [a]
toList (Leaf x) = [x]
toList (Node l r) = toList l ++ toList r

-- "There are no BSTs of size 15"
makeQuickCheckDoIt :: Property
makeQuickCheckDoIt = forAll (arbitrary :: Gen (BTree Int)) $ \xs ->
  let failing = satisfy xs 15.0 200 prop_isBST in
  whenFail (do
      putStrLn "The failing test: "
      print (fromJust failing)
    ) $ length (toList xs) >= 15 ==> not (isJust failing)

prop_isert :: Property
prop_isert = satisfyPrecondition isSorted $ \xs ->
  forAll (arbitrary :: Gen Int) $ \x -> isSorted (insert x xs)
