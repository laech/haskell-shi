{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Time.OffsetDateTime
  ( OffsetDateTime
  , module Data.Time.Base
  , module Data.Time.LocalDate
  , module Data.Time.LocalTime
  , module Data.Time.LocalDateTime
  , module Data.Time.Offset
  ) where

import Control.Monad.Fail
import Data.Ord
import Data.Time.Base
import Data.Time.LocalDate
import Data.Time.LocalDateTime
import Data.Time.LocalTime
import Data.Time.Offset

-- | A date and time with an offset.
data OffsetDateTime =
  OffsetDateTime {-# UNPACK #-}!Offset
                 !LocalDateTime
  deriving (Eq)

instance MonadFail m => FromDateTime (Offset -> m OffsetDateTime) where
  fromDateTime year month day hour minute second nano offset =
    OffsetDateTime offset <$>
    fromDateTime year month day hour minute second nano

instance FromLocalDateTime (Offset -> OffsetDateTime) where
  fromLocalDateTime = flip OffsetDateTime

modifyLocalDateTime' ::
     HasLocalDateTime a => (x -> LocalDateTime -> LocalDateTime) -> x -> a -> a
modifyLocalDateTime' f x = modifyLocalDateTime (f x . getLocalDateTime)

instance HasOffset OffsetDateTime where
  getOffset (OffsetDateTime offset _) = offset
  setOffset offset' o@(OffsetDateTime offset dt)
    | offset == offset' = o
    | otherwise = OffsetDateTime offset' dt

instance HasLocalDateTime OffsetDateTime where
  getLocalDateTime (OffsetDateTime _ dt) = dt
  setLocalDateTime dt o@(OffsetDateTime offset dt')
    | dt == dt' = o
    | otherwise = OffsetDateTime offset dt

instance HasLocalDate OffsetDateTime where
  getLocalDate = getLocalDate . getLocalDateTime
  setLocalDate = modifyLocalDateTime' setLocalDate

instance HasLocalTime OffsetDateTime where
  getLocalTime = getLocalTime . getLocalDateTime
  setLocalTime = modifyLocalDateTime' setLocalTime

instance HasYear OffsetDateTime where
  getYear = getYear . getLocalDateTime
  setYear = modifyLocalDateTime' setYear

instance HasMonth OffsetDateTime where
  getMonth = getMonth . getLocalDateTime
  setMonth = modifyLocalDateTime' setMonth
  addMonths = modifyLocalDateTime' addMonths

instance HasDayOfMonth OffsetDateTime where
  getDayOfMonth = getDayOfMonth . getLocalDateTime

instance HasDayOfYear OffsetDateTime where
  getDayOfYear = getDayOfYear . getLocalDateTime

instance AddDays OffsetDateTime where
  addDays = modifyLocalDateTime' addDays

instance HasHour OffsetDateTime where
  getHour = getHour . getLocalDateTime
  addHours = modifyLocalDateTime' addHours

instance HasMinute OffsetDateTime where
  getMinute = getMinute . getLocalDateTime
  addMinutes = modifyLocalDateTime' addMinutes

instance HasSecond OffsetDateTime where
  getSecond = getSecond . getLocalDateTime
  addSeconds = modifyLocalDateTime' addSeconds

instance HasSecondOfDay OffsetDateTime where
  getSecondOfDay = getSecondOfDay . getLocalDateTime

instance HasNanoOfSecond OffsetDateTime where
  getNanoOfSecond = getNanoOfSecond . getLocalDateTime
  addNanos = modifyLocalDateTime' addNanos

instance HasNanoOfDay OffsetDateTime where
  getNanoOfDay = getNanoOfDay . getLocalDateTime

getEpochLocalDateTime :: OffsetDateTime -> LocalDateTime
getEpochLocalDateTime (OffsetDateTime offset dt) =
  addSeconds (-(getTotalOffsetSeconds offset)) dt

instance HasEpochDay OffsetDateTime where
  getEpochDay = getEpochDay . getEpochLocalDateTime

instance HasEpochSecond OffsetDateTime where
  getEpochSecond = getEpochSecond . getEpochLocalDateTime

instance HasEpochMilli OffsetDateTime where
  getEpochMilli = getEpochMilli . getEpochLocalDateTime

instance Show OffsetDateTime where
  show (OffsetDateTime offset dt) = show dt ++ show offset

instance Ord OffsetDateTime where
  compare a b =
    case comparing getEpochLocalDateTime a b of
      EQ -> comparing getLocalDateTime a b
      ord -> ord
