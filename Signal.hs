{-# LANGUAGE DeriveFunctor #-}
module Signal where

import VBool

--------------------------------------------------------------------------------

data Sig a = Sig{ val :: a, next :: Maybe (Double,Sig a) }
 deriving (Eq, Show, Functor)

mkSig :: a -> [(Double,a)] -> Sig a
mkSig a []          = Sig a Nothing
mkSig a ((d,b):dbs) = Sig a (Just (d, mkSig b dbs))

delta :: Sig a -> Maybe Double
delta a = fst `fmap` next a

minDelta :: Maybe Double -> Maybe Double -> Maybe Double
minDelta Nothing  my       = my
minDelta mx       Nothing  = mx
minDelta (Just x) (Just y) = Just (x `min` y)

shift :: Double -> Sig a -> Sig a
shift d (Sig x (Just (t,a))) | d > 0 =
  if d < t
    then Sig x (Just (t-d,a))
    else shift (d-t) a
shift _ a                            = a

--------------------------------------------------------------------------------

instance Applicative Sig where
  pure x  = Sig x Nothing
  f <*> x = fmap (uncurry ($)) (zipp f x)

zipp :: Sig a -> Sig b -> Sig (a,b)
zipp a@(Sig x _) b@(Sig y _) = Sig (x,y) rxy
 where
  rxy = case delta a `minDelta` delta b of
          Nothing -> Nothing
          Just d  -> Just (d, zipp (shift d a) (shift d b))

--------------------------------------------------------------------------------

scan :: (Double -> a -> a -> a) -> Sig a -> Sig a
scan f (Sig x rx) =
  case rx of
    Nothing    -> Sig x Nothing
    Just (d,z) -> let a = scan f z in Sig (f d x (val a)) (Just (d, a))

always :: Sig VBool -> Sig VBool
always = scan op where op d x y = (x#d) &&+ y

eventually :: Sig VBool -> Sig VBool
eventually = scan op where op d x y = (x#d) ||+ y

--------------------------------------------------------------------------------

