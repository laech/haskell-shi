module Data.Time.LocalDateTime
  ( LocalDateTime(..)
  , module Data.Time.Base
  ) where

import Data.Time.Base
import Data.Time.LocalDate
import Data.Time.LocalTime

-- | Date and time without time zone.
data LocalDateTime =
  LocalDateTime LocalDate
                LocalTime
  deriving (Eq, Ord, Show)

instance HasEpochSecond LocalDateTime where
  getEpochSecond (LocalDateTime date time) =
    getEpochDay date * secondsPerDay + fromIntegral (getSecondOfDay time)
    where
      secondsPerDay = 24 * 60 * 60

instance HasNanoOfSecond LocalDateTime where
  getNanoOfSecond (LocalDateTime _ time) = getNanoOfSecond time
