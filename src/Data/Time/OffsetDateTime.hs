{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Time.OffsetDateTime
  ( OffsetDateTime
  , module Data.Time.Base
  , module Data.Time.LocalDateTime
  , module Data.Time.Offset
  ) where

import Control.Monad.Fail
import Data.Time.Base
import Data.Time.LocalDateTime
import Data.Time.Offset

-- | A date and time with an offset.
data OffsetDateTime =
  OffsetDateTime Offset
                 LocalDateTime
  deriving (Eq)

instance MonadFail m => FromDateTime (Offset -> m OffsetDateTime) where
  fromDateTime year month day hour minute second nano offset =
    OffsetDateTime offset <$>
    fromDateTime year month day hour minute second nano

instance FromLocalDateTime (Offset -> OffsetDateTime) where
  fromLocalDateTime = flip OffsetDateTime

instance Show OffsetDateTime where
  show (OffsetDateTime offset dt) = show dt ++ show offset
