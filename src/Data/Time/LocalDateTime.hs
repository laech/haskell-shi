module Data.Time.LocalDateTime
  ( LocalDateTime(..)
  , module Data.Time.Base
  ) where

import Data.Time.Base
import Data.Time.LocalDate
import Data.Time.LocalTime
import Data.Time.Month

-- | Date and time without time zone.
data LocalDateTime =
  LocalDateTime LocalDate
                LocalTime
  deriving (Eq, Ord)

instance HasLocalDate LocalDateTime where
  getLocalDate (LocalDateTime date _) = date

instance HasLocalTime LocalDateTime where
  getLocalTime (LocalDateTime _ time) = time

instance HasYear LocalDateTime where
  getYear (LocalDateTime date _) = getYear date
  addYears = addYears'
  setYear = setYear'

addYears' :: Int -> LocalDateTime -> LocalDateTime
addYears' n dt = setYear (getYear dt + fromIntegral n) dt

setYear' :: Integer -> LocalDateTime -> LocalDateTime
setYear' y dt@(LocalDateTime date time)
  | y == getYear dt = dt
  | otherwise = LocalDateTime (setYear y date) time

instance HasMonth LocalDateTime where
  getMonth (LocalDateTime date _) = getMonth date
  addMonths = addMonths'
  setMonth = setMonth'

addMonths' :: Int -> LocalDateTime -> LocalDateTime
addMonths' 0 dt = dt
addMonths' n (LocalDateTime date time) = LocalDateTime (addMonths n date) time

setMonth' :: Month -> LocalDateTime -> LocalDateTime
setMonth' month dt@(LocalDateTime date time)
  | month == getMonth dt = dt
  | otherwise = LocalDateTime (setMonth month date) time

instance HasDayOfMonth LocalDateTime where
  getDayOfMonth (LocalDateTime date _) = getDayOfMonth date

instance HasDayOfYear LocalDateTime where
  getDayOfYear = getDayOfYear'

getDayOfYear' :: LocalDateTime -> Int
getDayOfYear' (LocalDateTime date _) = getDayOfYear date

addDays' :: Int -> LocalDateTime -> LocalDateTime
addDays' 0 dt = dt
addDays' days (LocalDateTime date time) = LocalDateTime (addDays days date) time

addTime' :: Int -> Int -> Int -> Int -> LocalDateTime -> LocalDateTime
addTime' 0 0 0 0 dt = dt
addTime' hours minutes seconds nanos dt@(LocalDateTime date time) =
  let (days, time') = addTime hours minutes seconds nanos time
   in if days == 0 && time == time'
        then dt
        else LocalDateTime (addDays days date) time'

instance HasHour LocalDateTime where
  getHour (LocalDateTime _ time) = getHour time
  addHours n = addTime' n 0 0 0

instance HasMinute LocalDateTime where
  getMinute (LocalDateTime _ time) = getMinute time
  addMinutes n = addTime' 0 n 0 0

instance HasSecond LocalDateTime where
  getSecond (LocalDateTime _ time) = getSecond time
  addSeconds n = addTime' 0 0 n 0

instance HasNanoOfSecond LocalDateTime where
  getNanoOfSecond (LocalDateTime _ time) = getNanoOfSecond time
  addNanos = addTime' 0 0 0

instance HasSecondOfDay LocalDateTime where
  getSecondOfDay (LocalDateTime _ time) = getSecondOfDay time

instance HasEpochDay LocalDateTime where
  addDays = addDays'
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

instance Show LocalDateTime where
  show (LocalDateTime date time) = show date ++ "T" ++ show time
