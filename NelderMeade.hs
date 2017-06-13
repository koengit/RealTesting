module Main where

import Control.Monad
import Test.QuickCheck
import Numeric.GSL.Minimization
import Data.List

arg :: (a -> b) -> a
arg _ = undefined

p :: [Int] -> VBool
p xs =
  (  (length xs ==% 8)
 &&+ foldr (&&+) true
                 [ (0 <=% x) &&+ (x <=% 9)
                 | x <- xs
                 ]
 &&+ foldr (&&+) true
                 [ nt (x ==% y)
                 | (x,y) <- pairs xs
                 ]
  ) ==>%
  ( nt ((sem "SEND" + sem "MORE") ==% sem "MONEY")
  )
 where
  tab = "SENDMORY" `zip` xs

  sem = sem' . reverse
  
  sem' []     = 0
  sem' (c:cs) = head [ x | (c',x) <- tab, c' == c ] + 10 * sem' cs 

pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs
pairs []     = []

f ws = minimize NMSimplex2 0.1 500 box p' xs
 where
  box = replicate n 10.0
  xs  = take n (ws ++ repeat 0)

  n     = dim (arg p)
  p' xs = let a = decode xs in p a -- + encode a # xs

  _      # []     = 0
  (x:xs) # (y:ys) = abs (x-y) + (xs # ys)
  []     # ys     = [0] # ys

prop_f (Vec ws) =
  whenFail (do putStrLn ("xs=" ++ show xs)
               putStrLn ("as=" ++ show as)
               putStrLn ("p=" ++ show (p as))) $
    p as >= 1
 where
  (xs, _) = f ws
  as = decode xs :: [Int]

data Vec = Vec [Double]
 deriving ( Eq, Ord )

instance Show Vec where
  show (Vec xs) = show (take 5 xs) ++ ".."

instance Arbitrary Vec where
  arbitrary =
    do x         <- choose (0.0,10)
       ~(Vec xs) <- arbitrary
       return (Vec (x:xs))

type VBool = Double

false, true :: VBool
false = -1
true  = 1

nt :: VBool -> VBool
nt x = -x

(&&+), (||+), (<=>%) :: VBool -> VBool -> VBool
x &&+ y
  | signum x == signum y = x + y - signum x
  | otherwise            = x `min` y
x ||+ y = nt (nt x &&+ nt y)      
x <=>% y
  | (x >= 1) == (y >= 1) = true + (1000 / (0.1+abs (x-y)))
  | otherwise            = false - abs (x-y)

(==>%) :: VBool -> VBool -> VBool
x ==>% y
  | x >= 1    = y
  | otherwise = 100000 + nt x

(>%), (>=%), (<=%), (<%) :: Real a => a -> a -> VBool
x >=% y
  | x >= y    = true + toDouble (x-y)
  | otherwise = false - toDouble (y-x)
x >%  y = nt (x <=% y)
x <%  y = y >% x
x <=% y = y >=% x

(<==>%) :: [VBool] -> [VBool] -> VBool
[]     <==>% []     = true
(x:xs) <==>% (y:ys) = (x <=>% y) &&+ (xs <==>% ys)
xs     <==>% ys     = length (xs++ys) ==% 0

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational

class Object a where
  dim    :: a -> Int
  (==%)  :: a -> a -> VBool
  encode :: a -> [Double] 
  decode :: [Double] -> a

instance Object () where
  dim    _ = 0
  _ ==% _  = true
  encode _ = []
  decode _ = ()

instance Object Double where
  dim _ = 1
  x ==% y
    | x == y    = true
    | otherwise = false - abs (x-y)
  encode x     = [x]
  decode (x:_) = x

instance Object Int where
  dim _        = 1
  n ==% m | n == m    = true
          | otherwise = false - fromIntegral (abs (n-m))
  encode n     = [fromIntegral n]
  decode (x:_) = round x

instance (Object a, Object b) => Object (a, b) where
  dim (a,b) = dim a + dim b
  (a1,a2) ==% (b1,b2) = (a1 ==% b1) &&+ (a2 ==% b2)
  encode (a,b) = encode a ++ encode b
  decode xs    = (decode xs, b) where b = decode (drop (dim b) xs)

instance Object a => Object [a] where
  dim as = 1 + 10 * dim (head as)
  []     ==% []     = true
  (a:as) ==% (b:bs) = (a==%b) &&+ (as ==% bs)
  as     ==% bs     = false - fromIntegral (length (as++bs))
  encode as     | length as <= 10 = fromIntegral (length as) : concat (map encode as)
  decode (x:xs) = take (round x `min` 10) (decodeList xs)

decodeList :: Object a => [Double] -> [a]
decodeList xs = as
 where
  a  = head as
  as = go (dim a) xs

  go k xs
    | length xs >= k = decode xs : go k (drop k xs)
    | otherwise      = []


---

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
    [ nil | v >= 1 ]

  shrink (Star p) =
    [ p ] ++
    [ Star p' | p' <- shrink p ]

  shrink _ = []

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

(.*) :: VBool -> E -> E
v .* e | v >= 1    = e
       | otherwise = EpsNil v -- detta är alltid nil

data Intv = Int :-: Int
  deriving ( Eq, Ord, Show )

instance Arbitrary Intv where
  arbitrary =
    do i <- choose (0,3)
       j <- choose (i,4)
       return (i :-: j)

(/\) :: Intv -> Intv -> Intv
(i :-: j) /\ (i' :-: j') = (i `max` i') :-: (j `min` j')

notEmpty, ok :: Intv -> VBool
notEmpty (i :-: j) = i <=% j
ok       (i :-: j) = (0 <=% i) &&+ (i <=% j) &&+ (j <=% 7)

int :: Intv -> E -> E
int (i:-:j) p | i < 0 || j < i = nil
int (0:-:0) p = EpsNil true
int (0:-:j) p = EpsNil true :+: (p :>: int (0:-:(j-1)) p)
int (i:-:j) p = p :>: int ((i-1):-:(j-1)) p

{-
prop_e (e,e') (Vec ws) =
  whenFail (do putStrLn ("as=" ++ show as)
               putStrLn ("ijs=" ++ show (i1,i2))
               putStrLn ("rec1=" ++ show (rec (int i1 e :&: int i2 e') as))
               putStrLn ("rec2=" ++ show (rec (int (i1/\i2) (e :&: e')) as))) $
    ((eps e >= 1) == (eps e' >= 1)) ==>
      p a >= 1
 where
  opt = minimize NMSimplex2 0.1 500 box p' xs
  box = replicate n 10.0
  xs  = take n (ws ++ repeat 0)
  n   = dim a -- (arg p)

  p' xs = let a = decode xs in p a -- + encode a # xs

  p (((i,j),(i',j')),as) =
    ((i >=% 0) &&+ (j >=% i) &&+ (j <=% 15) &&+
     (i' >=% 0) &&+ (j' >=% i') &&+ (j' <=% 15) &&+
     (i'' <=% j'')) ==>%
      foldr (&&+) true
      [ rec (int i j e :&: int i' j' e') as' <=>%
          rec (int i'' j'' (e :&: e')) as'
      | as' <- tails as
      ]
   where
    i'' = i `max` i'
    j'' = j `min` j'

  a@(((i,j),(i',j')),as) = decode (fst opt)

  i'' = i `max` i'
  j'' = j `min` j'
-}
--

class Data a where
  vals :: a -> [Double]
  fill :: a -> [Double] -> a

instance Data () where
  vals _   = []
  fill _ _ = ()

instance (Data a, Data b) => Data (a,b) where
  vals (x,y)    = vals x ++ vals y
  fill (x,y) vs = (fill x (take k vs), fill y (drop k vs))
   where
    k = length (vals x)

instance Data Double where
  vals x       = [x]
  fill _ (v:_) = v

instance Data Int where
  vals n       = [fromIntegral n]
  fill _ (v:_) = round v

instance (Data a, Data b) => Data (Either a b) where
  vals (Left x)  = vals x
  vals (Right y) = vals y
  
  fill (Left  x) = Left  . fill x
  fill (Right y) = Right . fill y

instance Data Intv where
  vals (i:-:j) = vals (i,j)
  fill (i:-:j) = uncurry (:-:) . fill (i,j)

instance Data a => Data [a] where
  vals []     = []
  vals (x:xs) = vals (x,xs)
  
  fill []     vs = []
  fill (x:xs) vs = uncurry (:) (fill (x,xs) vs)

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

forData :: (Show a, Data a) => a -> (a -> VBool) -> Property
forData x h =
  whenFail (do print x'; putStrLn ("(" ++ show k ++ " iterations)")) $ h x' >= 1
 where
  vs   = vals x
  p ws = h (fill x ws)
  opt  = minimize NMSimplex2 1 100 box p vs
  box  = [ 15.0 | v <- vs ]
  x'   = fill x (fst opt)
  k    = length (lines (show (snd opt))) - 1

forData0 :: (Show a, Data a) => a -> (a -> VBool) -> Property
forData0 x h =
  whenFail (print x) $ h x >= 1

prop_e2 args@(((e,e'),as),(i,i')) =
  forData args $ \(((e,e'),as),(i,i')) ->
        ( (eps e <=>% eps e')
      &&+ ok i
      &&+ ok i'
      &&+ notEmpty (i/\i')
        ) ==>%
        (rec (int i e :&: int i' e') as <=>%
          rec (int (i/\i') (e :&: e')) as)

prop_e3 args =
  forData0 args $ \(((a,b),(c,d)),as) ->
    (recs ((a :&: b) :>: (c :&: d)) as <==>%
       recs ((a :>: c) :&: (b :>: d)) as)

main = quickCheckWith stdArgs{ maxSuccess = 10000 } prop_e3

