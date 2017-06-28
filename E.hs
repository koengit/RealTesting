module Main where

{-
Applying Data.hs to regular expression testing.
-}

import Test.QuickCheck
import Control.Monad( liftM, liftM2 )
import VBool
import Data

--------------------------------------------------------------------------------

data E
  = E :+: E
  | E :&: E
  | E :>: E
  | EpsNil VBool -- either eps (true) or nil (false)
  | C Int
  | Star E
 deriving ( Eq, Ord, Show )

instance Arbitrary E where
  arbitrary = sized arb
   where
    arb n = frequency
            [ (p, liftM2 (:+:) arb2 arb2)
            , (p, liftM2 (:>:) arb2 arb2)
            , (p, liftM Star arb1)
            , (2, liftM C arbitrary)
            , (1, return nil)
            , (1, return (EpsNil true))
            ]
     where
      p    = 3 `min` n
      arb2 = arb (n `div` 2)
      arb1 = arb (n-1)

  shrink (p :+: q) =
    [ p, q ] ++
    [ p' :+: q  | p' <- shrink p ] ++
    [ p  :+: q' | q' <- shrink q ]

  shrink (p :&: q) =
    [ p, q ] ++
    [ p' :&: q  | p' <- shrink p ] ++
    [ p  :&: q' | q' <- shrink q ]

  shrink (p :>: q) =
    [ p, q ] ++
    [ p' :>: q  | p' <- shrink p ] ++
    [ p  :>: q' | q' <- shrink q ]

  shrink (EpsNil v) =
    [ nil | isTrue v ]

  shrink (Star p) =
    [ p, emp ] ++
    [ Star p' | p' <- shrink p ]

  shrink (C _) =
    [ emp, nil ]

instance Data E where
  vals (p :+: q) = vals p ++ vals q
  vals (p :&: q) = vals p ++ vals q
  vals (p :>: q) = vals p ++ vals q
  vals (C n)     = vals n
  vals (Star p)  = vals p
  vals _         = []

  fill (p :+: q) = uncurry (:+:) . fill (p,q)
  fill (p :&: q) = uncurry (:&:) . fill (p,q)
  fill (p :>: q) = uncurry (:>:) . fill (p,q)
  fill (C n)     = C . fill n
  fill (Star p)  = Star . fill p
  fill p         = const p

(.*) :: VBool -> E -> E
v .* e | isTrue v  = e
       | otherwise = EpsNil v -- this is always nil

--------------------------------------------------------------------------------

emp, nil :: E
emp = EpsNil true
nil = EpsNil false

rec :: E -> [Int] -> VBool
rec e []     = eps e
rec e (x:xs) = rec (step e x) xs

recs :: E -> [Int] -> [VBool]
recs e xs = eps e : if null xs then [] else recs (step e (head xs)) (tail xs)

step :: E -> Int -> E
step (p :+: q) x = step p x :+: step q x
step (p :&: q) x = step p x :&: step q x
step (p :>: q) x = (eps p .* step q x) :+: (step p x :>: q)
step (Star p)  x = step p x :>: Star p
step (C c)     x = EpsNil (c ==% x)
step _         x = nil

eps :: E -> VBool
eps (p :+: q)  = eps p ||+ eps q
eps (p :&: q)  = eps p &&+ eps q
eps (p :>: q)  = eps p &&+ eps q
eps (EpsNil v) = v
eps (C _)      = false
eps (Star _)   = true

--------------------------------------------------------------------------------
-- property 1: interval intersection

data Intv = Int :-: Int
  deriving ( Eq, Ord, Show )

instance Arbitrary Intv where
  arbitrary =
    do i <- choose (0,3)
       j <- choose (i,4)
       return (i :-: j)

instance Data Intv where
  vals (i:-:j) = vals (i,j)
  fill (i:-:j) = uncurry (:-:) . fill (i,j)

(/\) :: Intv -> Intv -> Intv
(i :-: j) /\ (i' :-: j') = (i `max` i') :-: (j `min` j')

notEmpty, ok :: Intv -> VBool
notEmpty (i :-: j) = i <=% j
ok       (i :-: j) = (0 <=% i) &&+ (i <=% j) &&+ (j <=% 3)

int :: Intv -> E -> E
int (i:-:j) p | i < 0 || j < i = nil
int (0:-:0) p = EpsNil true
int (0:-:j) p = EpsNil true :+: (p :>: int (0:-:(j-1)) p)
int (i:-:j) p = p :>: int ((i-1):-:(j-1)) p

prop_IntervalIntersection args =
  forData0 args $ \(((e,e'),List as _ _),(i,i')) ->
        ( (eps e ==% eps e')
      &&+ ok i
      &&+ ok i'
      &&+ notEmpty (i/\i')
        ) ==>%
        (recs (int i e :&: int i' e') as ==%
          recs (int (i/\i') (e :&: e')) as)

--------------------------------------------------------------------------------
-- property 2: sequential composition intersection

prop_SeqIntersection args =
  forData0 args $ \(((a,b),(c,d)),List as _ _) ->
    (recs ((a :&: b) :>: (c :&: d)) as ==%
       recs ((a :>: c) :&: (b :>: d)) as)

prop_SeqIntersection' args =
  forData args $ \((a,b),List as _ _) ->
    (recs ((a :&: b) :>: (a :&: b)) as ==%
       recs ((a :>: a) :&: (b :>: b)) as)

--------------------------------------------------------------------------------
-- property 2: sequential composition intersection

prop_NoPalindromes args =
  forData args $ \(List as _ _) ->
    (length as >=% 5 &&+ foldr (&&+) true [ nt (a ==% b) | (a,b) <- pairs (take (length as `div` 2) as) ]) ==>%
      nt (as ==% reverse (as :: [Int]))

pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs
pairs _      = []

--------------------------------------------------------------------------------
-- main

main =
  quickCheckWith stdArgs{ maxSuccess = 10000 }
    --prop_IntervalIntersection
    prop_SeqIntersection'
    --prop_NoPalindromes

--------------------------------------------------------------------------------

