module Object where

{-
Coding discrete objects as vectors of booleans.
-}

import Test.QuickCheck
import Numeric.GSL.Minimization
import VBool

--------------------------------------------------------------------------------

class Object a where
  dim    :: a -> Int
  encode :: a -> [Double] 
  decode :: [Double] -> a

instance Object () where
  dim    _ = 0
  encode _ = []
  decode _ = ()

instance Object Double where
  dim    _     = 1
  encode x     = [x]
  decode (x:_) = x

instance Object Int where
  dim _        = 1
  encode n     = [fromIntegral n]
  decode (x:_) = round x

instance (Object a, Object b) => Object (a, b) where
  dim (a,b)    = dim a + dim b
  encode (a,b) = encode a ++ encode b
  decode xs    = (a, decode (drop (dim a) xs)) where a = decode xs

instance Object a => Object [a] where
  dim as = 1 + 10 * dim (head as)
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

--------------------------------------------------------------------------------

forObject :: (Show a, Object a) => (a -> VBool) -> Property
forObject h =
  forAll (Blind `fmap` doubles) $ \(Blind ws) ->
    let opt  = minimize NMSimplex2 1 100 box p vs
        box  = [ 15.0 | v <- vs ]
        vs   = take (dim (arg h)) ws
        p xs = howTrue (h (decode xs))
        x    = decode (fst opt) `asTypeOf` arg h
        k    = length (lines (show (snd opt))) - 1
     in whenFail (do print x; putStrLn ("(" ++ show k ++ " iterations)")) $
          isTrue (h x)
 where
  arg :: (a -> b) -> a
  arg _ = error "arg"
 
  doubles = do x  <- arbitrary
               xs <- doubles
               return (x:xs)

prop_Palindrome :: [Int] -> VBool
prop_Palindrome xs =
  ( foldr (&&+) true [ nt (x ==% y) | (x,y) <- pairs (take (length xs `div` 2) xs) ] &&+
    length xs >=% 10 ) ==>%
    nt (reverse xs ==% xs)

pairs []     = []
pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs


--------------------------------------------------------------------------------

