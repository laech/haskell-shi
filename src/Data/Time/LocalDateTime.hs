{-# LANGUAGE FlexibleInstances #-}

module Data.Time.LocalDateTime
  ( LocalDateTime
  , HasLocalDateTime
  , FromLocalDateTime(..)
  , module Data.Time.Base
  ) where

import Control.Monad.Fail
import Data.Time.Base
import Data.Time.LocalDate
import Data.Time.LocalTime

-- | Date and time without time zone.
data LocalDateTime =
  LocalDateTime LocalDate
                LocalTime
  deriving (Eq, Ord)

class (HasLocalDate a, HasLocalTime a, HasEpochSecond a, HasEpochMilli a) =>
      HasLocalDateTime a
  where
  getLocalDateTime :: a -> LocalDateTime

instance HasLocalDateTime LocalDateTime where
  getLocalDateTime = id

class FromLocalDateTime a where
  fromLocalDateTime :: LocalDateTime -> a
  fromLocalDateAndTime :: LocalDate -> LocalTime -> a
  fromLocalDateAndTime date time = fromLocalDateTime $ LocalDateTime date time

instance FromLocalDateTime LocalDateTime where
  fromLocalDateTime = id

instance MonadFail m => FromDateTime (m LocalDateTime) where
  fromDateTime year month day hour minute second nano =
    LocalDateTime <$> dateM <*> timeM
    where
      dateM = fromDate year month day
      timeM = fromTime hour minute second nano

instance HasLocalDate LocalDateTime where
  getLocalDate (LocalDateTime date _) = date
  setLocalDate date dt@(LocalDateTime date' time)
    | date == date' = dt
    | otherwise = fromLocalDateAndTime date time

instance HasLocalTime LocalDateTime where
  getLocalTime (LocalDateTime _ time) = time
  setLocalTime time dt@(LocalDateTime date time')
    | time == time' = dt
    | otherwise = fromLocalDateAndTime date time

instance HasYear LocalDateTime where
  getYear = getYear . getLocalDate
  setYear y = modifyLocalDate (setYear y . getLocalDate)

instance HasMonth LocalDateTime where
  getMonth = getMonth . getLocalDate
  addMonths n = modifyLocalDate (addMonths n . getLocalDate)
  setMonth m = modifyLocalDate (setMonth m . getLocalDate)

instance HasDayOfMonth LocalDateTime where
  getDayOfMonth = getDayOfMonth . getLocalDate
  addDays n = modifyLocalDate (addDays n . getLocalDate)

instance HasDayOfYear LocalDateTime where
  getDayOfYear = getDayOfYear . getLocalDate

addTime' :: Int -> Int -> Int -> Int -> LocalDateTime -> LocalDateTime
addTime' 0 0 0 0 dt = dt
addTime' hours minutes seconds nanos dt =
  let (days, time') = addTime hours minutes seconds nanos (getLocalTime dt)
   in addDays days . setLocalTime time' $ dt

instance HasHour LocalDateTime where
  getHour = getHour . getLocalTime
  addHours n = addTime' n 0 0 0

instance HasMinute LocalDateTime where
  getMinute = getMinute . getLocalTime
  addMinutes n = addTime' 0 n 0 0

instance HasSecond LocalDateTime where
  getSecond = getSecond . getLocalTime
  addSeconds n = addTime' 0 0 n 0

instance HasNanoOfSecond LocalDateTime where
  getNanoOfSecond = getNanoOfSecond . getLocalTime
  addNanos = addTime' 0 0 0

instance HasSecondOfDay LocalDateTime where
  getSecondOfDay = getSecondOfDay . getLocalTime

instance HasNanoOfDay LocalDateTime where
  getNanoOfDay = getNanoOfDay . getLocalTime

instance HasEpochDay LocalDateTime where
  getEpochDay = getEpochDay . getLocalDate

instance HasEpochSecond LocalDateTime where
  getEpochSecond dt =
    getEpochDay dt * secondsPerDay + fromIntegral (getSecondOfDay dt)
    where
      secondsPerDay = 24 * 60 * 60

instance HasEpochMilli LocalDateTime where
  getEpochMilli datetime =
    getEpochSecond datetime * 1000 +
    fromIntegral (getNanoOfSecond datetime) `div` 1000000

instance Show LocalDateTime where
  show dt = show (getLocalDate dt) ++ "T" ++ show (getLocalTime dt)
