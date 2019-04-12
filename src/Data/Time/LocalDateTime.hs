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

instance HasYear LocalDateTime where
  getYear (LocalDateTime date _) = getYear date

instance HasMonth LocalDateTime where
  getMonth (LocalDateTime date _) = getMonth date

instance HasDayOfMonth LocalDateTime where
  getDayOfMonth (LocalDateTime date _) = getDayOfMonth date

instance HasDayOfYear LocalDateTime where

  getDayOfYear (LocalDateTime date _) = getDayOfYear date

  addDays 0 dt = dt
  addDays days (LocalDateTime date time) =
    LocalDateTime (addDays days date) time

instance HasHour LocalDateTime where
  getHour (LocalDateTime _ time) = getHour time

instance HasMinute LocalDateTime where
  getMinute (LocalDateTime _ time) = getMinute time

instance HasSecond LocalDateTime where
  getSecond (LocalDateTime _ time) = getSecond time

instance HasNanoOfSecond LocalDateTime where
  getNanoOfSecond (LocalDateTime _ time) = getNanoOfSecond time

instance HasSecondOfDay LocalDateTime where
  getSecondOfDay (LocalDateTime _ time) = getSecondOfDay time

instance HasEpochDay LocalDateTime where
  getEpochDay (LocalDateTime date _) = getEpochDay date

instance HasEpochSecond LocalDateTime where
  getEpochSecond (LocalDateTime date time) =
    getEpochDay date * secondsPerDay + fromIntegral (getSecondOfDay time)
    where
      secondsPerDay = 24 * 60 * 60

instance HasEpochMilli LocalDateTime where
  getEpochMilli datetime =
    getEpochSecond datetime * 1000 +
    fromIntegral (getNanoOfSecond datetime) `div` 1000000
