{-# LANGUAGE DeriveGeneric #-}
module Val where

import qualified Data.Map as M
import Data.List( sort, sortBy, intercalate )
import Data.Ord
import VBool
import Data
import Test.QuickCheck
import Badness
import System.Process
import GHC.Generics( Generic )
import qualified Process

newtype Val a = Val { vals :: [(a,VBool)] }
 deriving ( Eq, Ord, Show )

val :: a -> Val a
val x = Val [(x,true)]

the :: Val a -> a
the (Val xs) = head [ x | (x,v) <- xs, isTrue v ]

mkVal :: Ord a => [(a,VBool)] -> Val a
mkVal = Val . M.toList . M.fromListWith (||+)

vbool :: VBool -> Val Bool
vbool v =
  Val [ (False, nt v)
      , (True,  v)
      ]

class Choice a where
  ifThenElse :: Val Bool -> a -> a -> a

instance Choice Double where
  ifThenElse c x y =
    if the c then x else y -- loses VBool info

instance Ord a => Choice (Val a) where
  ifThenElse (Val cs) (Val xs) (Val ys) =
    mkVal
    [ (z,cv &&+ zv)
    | (c,cv) <- cs
    , (z,zv) <- if c then xs else ys
    ]

mapVal :: Ord b => (a->b) -> Val a -> Val b
mapVal f (Val xs) =
  mkVal
  [ (f x, a)
  | (x,a) <- xs
  ]

liftVal :: Ord c => (a->b->c) -> Val a -> Val b -> Val c
liftVal f (Val xs) (Val ys) =
  mkVal
  [ (f x y, a &&+ b)
  | (x,a) <- xs
  , (y,b) <- ys
  ]

(||?), (&&?) :: Val Bool -> Val Bool -> Val Bool
(||?) = liftVal (||)
(&&?) = liftVal (&&)
nott  = mapVal not

instance (Ord a, Num a) => Num (Val a) where
  (+)         = liftVal (+)
  (-)         = liftVal (-)
  (*)         = liftVal (*)
  abs         = mapVal abs
  negate      = mapVal negate
  signum      = mapVal signum
  fromInteger = val . fromInteger

instance (Ord a, Fractional a) => Fractional (Val a) where
  (/)          = liftVal (/)
  recip        = mapVal recip
  fromRational = val . fromRational

instance (Ord a, Floating a) => Floating (Val a) where
  pi = val pi
  exp = mapVal exp
  log = mapVal log
  sin = mapVal sin
  cos = mapVal cos
  asin = mapVal asin
  acos = mapVal acos
  atan = mapVal atan
  sinh = mapVal sinh
  cosh = mapVal cosh
  asinh = mapVal asinh
  acosh = mapVal acosh
  atanh = mapVal atanh

class VCompare a where
  (==?), (/=?), (>?), (>=?), (<?), (<=?) :: a -> a -> Val Bool

instance VCompare Double where
  (==?) = compDouble (==%)
  (/=?) = compDouble (\x y -> nt (x ==% y))
  (>?)  = compDouble (>%)
  (>=?) = compDouble (>=%)
  (<?)  = compDouble (<%)
  (<=?) = compDouble (<=%)

compDouble op x y =
  vbool (x `op` y)

instance VCompare a => VCompare (Val a) where
  (==?) = compVal (==?)
  (/=?) = compVal (/=?)
  (>?)  = compVal (>?)
  (>=?) = compVal (>=?)
  (<?)  = compVal (<?)
  (<=?) = compVal (<=?)

compVal op x y =
  smash (liftVal op x y)

smash :: Ord a => Val (Val a) -> Val a -- monadic join
smash (Val vs) =
  mkVal
  [ (w, a &&+ b)
  | (Val ws,a) <- vs
  , (w     ,b) <- ws
  ]

--------------------------------------------------------------------------------

forget :: (Ord a, Ord b) => (a -> b) -> Val a -> Val a
forget badness (Val xs) = Val (sort (take 100 (reverse (sortBy (comparing best) xs))))
 where
  best (x,a) = (isTrue a, badness x, howTrue a)

propVal :: Val Bool -> VBool
propVal (Val bs) = foldr1 (&&+) [ if b then v else nt v | (b,v) <- bs ]

propVal0 :: Val Bool -> VBool
propVal0 (Val bs) = head [ if isTrue v then good 5 else bad 5 | (True,v) <- bs ]

--------------------------------------------------------------------------------

instance Process.Valued Val where
  val = val
  vmap = mapVal
  vlift = liftVal
  vbind x f = smash (mapVal f x)
  vfail = error
  vifThenElse = ifThenElse
  vprune = forget
  veq = (==?)
  vgeq = (>=?)

--------------------------------------------------------------------------------

f :: (Num a, VCompare a, Choice a) => a -> a
f x =
  ifThenElse (x <? a) 10
    (ifThenElse (x >? (a+d)) 20
      (-10))

f0 :: (Fractional a, Num a, VCompare a, Choice a) => a -> a
f0 x = 0.01*x^2 -x +25 - 0.000013*x^3

f1 :: (Fractional a, Num a, VCompare a, Choice a) => a -> a
f1 x =
  ifThenElse ((x <? 200) ||? (x >? 300)) (0.01*x^2 -x +25 - 0.000013*x^3) $
  (0.01*x^2 -x -20 - 0.000013*x^3)

f2 :: (Fractional a, Floating a, Num a, VCompare a, Choice a) => a -> a
f2 x = sin ((x-50) / 45) * (x / 200) + 1.2

f3 :: (Fractional a, Floating a, Num a, VCompare a, Choice a) => a -> a
f3 x =
  (ifThenElse ((x <? 200) ||? (x >? 300)) 1 $
    sin (x * 0.02 *pi + 0.5*pi)
  ) * 100 + 99

f4 :: (Fractional a, Floating a, Num a, VCompare a, Choice a) => a -> a
f4 x =
  (ifThenElse (x <? 200) 1 $
   ifThenElse (x >? 300) 1 $
    sin (x * 0.02 *pi + 0.5*pi)
  ) * 100 + 99

g :: (Fractional a, VCompare a, Choice a) => a -> a
g x =
  ifThenElse (x <? a) (10 `max` x)
    (ifThenElse (x >? (a+d)) (10 `max` (2*a+d - x))
      (-10))
 where
  x `max` y =
    ifThenElse (x >=? y) x y

h :: (Num a, VCompare a, Choice a) => a -> a
h x =
  ifThenElse (x <=? a) 10 $
  ifThenElse (x <=? (a+d)) (-10) $
  ifThenElse (x <=? (2*a)) 20 $
  ifThenElse (x <=? (2*a+d)) (-10) $
    15

a, d :: Num a => a
a = 202
d = 1

prop_Basic f x =
  withBadness $
  let (y,b) = forData x (\x -> propVal (f (x :: Double) >=? 0)) in
    whenFail (print y) (isTrue b)

prop_Val f x =
  withBadness $
  let (y,b) = forData x (\x -> propVal (f (val x) >=? 0)) in
    whenFail (print y) (isTrue b)

plot :: (Double,Double) -> [(String, Double -> Double)] -> IO ()
plot (xL,xR) fs =
  do sequence_
       [ writeFile ("plot-" ++ name ++ ".xy") $ unlines $
           [ show x ++ " " ++ show y
           | (x,y) <- xs `zip` ys
           ]
       | ((name,_),ys) <- fs `zip` yss
       ]
     writeFile "plot.in" $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       --, "set yrange [0:]"
       , "set autoscale x"
       , "set yrange [" ++ show miny ++ ":" ++ show maxy ++ "]"
       , "set output 'plot.pdf'"
       ] ++
       [ "plot " ++
         intercalate ", "
         [ "'plot-" ++ name ++ ".xy' with lines title '" ++ name ++ "'"
         | (name,_) <- fs
         ]
       ]
     system "gnuplot < plot.in"
     return ()
 where
  dx  = (xR-xL) / 1000
  xs  = [xL,xL+dx..xR]
  yss = [ map f xs | (_,f) <- fs ]

  minY = minimum (concat yss)
  maxY = maximum (concat yss)
  dy   = (maxY-minY) / 33 -- 3%
  miny = minY-dy
  maxy = maxY+dy

plotf f fR = plot (-100,500)
             [ ("f", f)
             , ("fT", \x -> howTrue (propVal (fR (val x) >=? 0)))
             ]

plot3D :: (Double,Double) -> (Double,Double)
       -> [(String, (Double,Double) -> Double)] -> IO ()
plot3D (xL,xR) (yL,yR) fs =
  do sequence_
       [ writeFile ("plot-" ++ name ++ ".xyz") $ unlines $
           [ show x ++ " " ++ show y ++ " " ++ show z
           | ((x,y),z) <- xys `zip` zs
           ]
       | ((name,_),zs) <- fs `zip` zss
       ]
     writeFile "plot.in" $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       , "set autoscale xy"
       , "set zrange [" ++ show minz ++ ":" ++ show maxz ++ "]"
       , "set output 'plot3d.pdf'"
       , "set dgrid3d 30,30"
       , "set hidden3d"
       ] ++
       [ "splot " ++
         intercalate ", "
         [ "'plot-" ++ name ++ ".xyz' with lines title '" ++ name ++ "'"
         | (name,_) <- fs
         ]
       ]
     system "gnuplot < plot.in"
     return ()
 where
  dx  = (xR-xL) / 30
  dy  = (yR-yL) / 30
  xs  = [xL,xL+dx..xR]
  ys  = [yL,yL+dy..yR]
  xys = [ (x,y) | x <- xs, y <- ys ]
  zss = [ map f xys | (_,f) <- fs ]

  minZ = minimum (concat zss)
  maxZ = maximum (concat zss)
  dz   = (maxZ-minZ) / 33 -- 3%
  minz = minZ-dz
  maxz = maxZ+dz

----

data State = A | B | C deriving ( Show, Eq, Ord )

ship :: [Val Double] -> [Val State]
ship as = ss
 where
  vs = zipWith (+) (pre 0 vs) as
  xs = zipWith (+) (pre 0 xs) vs
  ss = [ ifThenElse (x <=? (-100)) (val A) $
           ifThenElse (x >=? 100) (val C)
             (val B)
       | x <- xs
       ]

observer :: [Val State] -> [Val Bool]
observer ss = ok
 where
  seenA = zipWith (||?)
            (pre (vbool false) seenA)
            (map (liftVal (==) (val A)) ss)
  seenC = zipWith (||?)
            (pre (vbool false) seenC)
            (map (liftVal (==) (val C)) ss)
  ok    = [ liftVal (/=) s s' &&?
            sA &&?
            sC
          | ((s,s'),(sA,sC)) <- (ss `zip` pre (val B) ss) `zip`
                                (pre (vbool false) seenA
                                  `zip` pre (vbool false) seenC)
          ]

ship2 :: [Val Double] -> [Val State]
ship2 as = ss
 where
  vs = zipWith (+) (pre 0 vs) as
  xs = zipWith (+) (pre 0 xs) vs
  ss = map (forget (const ())) $ pre (val B) (zipWith h xs ss)

  h x s =
    ifThenElse (x <=? (-100)) (val A) $
    ifThenElse (x >=? 100)    (val C) $
      s

pre x xs = x:xs

data Ps
  = Piece Int Double Ps
  | End
 deriving ( Eq, Show, Ord, Generic )

instance Data Ps

instance Arbitrary Ps where
  arbitrary =
    do k <- return 4 -- arbitrary
       dxs <- sequence [ do d <- choose (0,50)
                            x <- choose (-1,1)
                            return (d,x)
                       | i <- [0..k::Int]
                       ]
       return (foldr (uncurry Piece) End dxs)

  shrink (Piece k x q) =
    []
    {-
    [ q ] ++
    [ Piece (k+k') z q
    | k > 0
    , Piece k' y _ <- [q]
    , k' > 0
    , z <- [ (fromIntegral k * x + fromIntegral k' * y)/fromIntegral (k+k')
           ]
    ] ++
    [ Piece k x q'
    | q' <- shrink q
    ]
    -}

pieces :: Ps -> [Double]
pieces End             = []
pieces (Piece _ x End) = repeat x
pieces (Piece k x q)   = replicate k x ++ pieces q

values :: Ps -> [Double]
values End           = []
values (Piece _ x q) = x : values q

f_Ship pv bs =
  foldr (&&+) true [ (-1) <=% x &&+ x <=% 1 | x <- values bs ] ==>%
    (pv $ nott $ foldr (||?) (vbool false) ps)
 where
  ps = observer (ship (map val (take 80 (pieces bs))))

prop_Ship bs =
  withBadness $
    let (y,b) = forData bs (f_Ship propVal)
     in whenFail (do print y; print b) $
          isTrue $
            b

main1 = quickCheck prop_Ship

main2 = plot (-1,1)
        [ ("p",  \a -> howTrue $ f_Ship propVal0 $ accs a)
        -- , ("pT", \a -> howTrue $ f_Ship propVal  $ accs a)
        ]

accs a = Piece 15 (a-1)
       $ Piece 50 a
       $ Piece 60 (-1)
       $ End

prop_Ship' a =
  withBadness $
    let (y,b) = forData a (\a -> f_Ship propVal (accs a))
     in whenFail (print y) $
          isTrue $
            b

main3 = quickCheck prop_Ship'

--main = main2
main = main1
--main = do main2 --; main3
