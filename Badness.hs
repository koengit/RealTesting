-- A hack for badness-preserving shrinking in QuickCheck.
{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Badness where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.State
import Test.QuickCheck.Text
import Test.QuickCheck.Gen.Unsafe
import Data.IORef
import Data.Reflection

-- Try quickCheck (withBadness prop_sameDiv1000).
prop_sameDiv1000 :: Badness -> NonNegative (Large Int) -> NonNegative (Large Int) -> Property
prop_sameDiv1000 bad (NonNegative (Large x)) (NonNegative (Large y)) =
  badness bad (fromIntegral (abs (x `mod` 1000 - y `mod` 1000))) $
  x `div` 1000 === y `div` 1000

newtype Badness = Badness (IORef Double)

-- Get a Badness token. Should be used outside of any quantifiers.
withBadness :: Testable prop => (Given Badness => prop) -> Property
withBadness prop =
  ioProperty $ do
    ref <- newIORef (-1/0)
    return $ showBadness ref $ give (Badness ref) prop
  where
    showBadness ref =
      callback $ PostFinalFailure Counterexample $ \st _ -> do
        val <- readIORef ref
        putLine (terminal st) ("Badness: " ++ show val)

    -- This is the definition of ioProperty from QC <2.10.
    -- (The version from 2.10 disables shrinking in the inner property.)
    -- We rely on the fact that the I/O is not re-executed when the
    -- inner property is shrunk.
    ioProperty =
      MkProperty . fmap (MkProp . ioRose . fmap unProp) .
      promote . fmap (unProperty . property)

-- Set the badness for the current property, if it fails.
badness :: Testable prop => Badness -> Double -> prop -> Property
badness (Badness ref) x prop =
  ioProperty $ do
    val <- readIORef ref
    return $
      whenFail' (modifyIORef' ref (max x)) $
        x >= val ==> prop
