{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Time.Format
  ( Format(..)
  , format
  ) where

import Control.Monad.Fail
import Data.Time.Base
import Prelude hiding (fail)

data Format where
  Literal :: String -> Format
  MinDigits :: (Show a, Integral a) => Int -> TimeField a -> Format
  Field :: (Show a, Integral a) => TimeField a -> Format

signedPad :: (Integral a, Show a) => Int -> a -> String
signedPad minLength number = sign ++ (pad . show . abs $ number)
  where
    sign =
      if number < 0
        then "-"
        else ""
    pad str =
      if length str >= minLength
        then str
        else replicate (minLength - length str) '0' ++ str

format1 :: (MonadFail m, HasField a) => Format -> a -> m String
format1 (Field a) x =
  show <$> maybe (fail $ "No such field: " ++ show a) pure (get a x)
format1 (Literal s) _ = pure s
format1 (MinDigits n a) x =
  signedPad n <$> maybe (fail $ "No such field: " ++ show a) pure (get a x)

format :: HasField a => [Format] -> a -> Maybe String
format fs x = foldMap (`format1` x) fs
