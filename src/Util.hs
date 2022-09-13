{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Util (module Util, module Data.Maybe) where

import Data.Map qualified as M
import Data.Maybe
import GHC.Exts
import GHC.Word
import Text.PrettyPrint (Doc)

headMay [] = Nothing
headMay (x : xs) = Just x

lastMay [] = Nothing
lastMay xs = Just (last xs)

allEq xs = and $ zipWith (==) xs (drop 1 xs)

runUntilNoChange :: Eq p => (p -> p) -> p -> p
runUntilNoChange f d = if d /= result then runUntilNoChange f result else result
  where
    result = f d

lookupDef :: (Ord k, Monoid m) => k -> M.Map k m -> m
lookupDef i m = fromMaybe mempty (M.lookup i m)

foreign import prim "double2WordBwzh"
  double2WordBitwise# :: Double# -> Word#

foreign import prim "float2WordBwzh"
  float2WordBitwise# :: Float# -> Word#

{-# INLINE double2WordBitwise #-}
double2WordBitwise :: Double -> Word
double2WordBitwise (D# d) = W# (double2WordBitwise# d)

{-# INLINE float2WordBitwise #-}
float2WordBitwise :: Float -> Word
float2WordBitwise (F# f) = W# (float2WordBitwise# f)

instance MonadFail (Either String) where
  fail = Left

class Print a where
  write :: a -> Doc

(!!!) map x = case M.lookup x map of
  Nothing -> error (show map ++ show x)
  Just x -> x