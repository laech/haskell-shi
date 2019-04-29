{-# LANGUAGE FlexibleInstances #-}

module Data.Time.LocalDate
  ( LocalDate
  , HasLocalDate(..)
  , modifyLocalDate
  , module Data.Time.Base
  ) where

import Control.Monad.Fail
import Data.Maybe
import Data.Time.Base
import Data.Time.Month
import Data.Time.Year
import Data.Word
import Prelude hiding (fail)

-- | A date without time zone.
data LocalDate =
  LocalDate Integer
            Word8
            Word8
  deriving (Eq, Ord)

class (HasYear a, HasMonth a, HasDayOfMonth a, HasDayOfYear a, HasEpochDay a) =>
      HasLocalDate a
  where
  getLocalDate :: a -> LocalDate
  setLocalDate :: LocalDate -> a -> a

-- | Calls 'setLocalDate' with a new date obtained by apply a function
-- to the old date.
modifyLocalDate :: HasLocalDate a => (LocalDate -> LocalDate) -> a -> a
modifyLocalDate f x = setLocalDate (f (getLocalDate x)) x

instance HasLocalDate LocalDate where
  getLocalDate = id
  setLocalDate = const

instance FromEpochDay LocalDate where
  fromEpochDay = fromEpochDay'

fromEpochDay' :: Integer -> LocalDate
fromEpochDay' epochDay =
  let days0000To1970 = 719528
      days400Years = 146097
      day0' = epochDay + days0000To1970 - 60
      (day0, adjust) =
        if day0' < 0
          then let cycles = (day0' + 1) `div` days400Years - 1
                in (day0' + (-cycles) * days400Years, cycles * 400)
          else (day0', 0)
      yearEst' = (400 * day0 + 591) `div` days400Years
      doyEst' =
        day0 -
        (365 * yearEst' + yearEst' `div` 4 - yearEst' `div` 100 +
         yearEst' `div` 400)
      (yearEst, doyEst) =
        if doyEst' >= 0
          then (yearEst', doyEst')
          else let y = yearEst' - 1
                in (y, day0 - (365 * y + y `div` 4 - y `div` 100 + y `div` 400))
      marchMonth0 = (doyEst * 5 + 2) `div` 153
      month = (marchMonth0 + 2) `rem` 12 + 1
      dom = doyEst - (marchMonth0 * 306 + 5) `div` 10 + 1
      year = yearEst + adjust + marchMonth0 `div` 10
   in fromJust (fromDate year (fromIntegral month) (fromIntegral dom))
  -- Ported from java.time.LocalDate.ofEpochDay

instance MonadFail m => FromYearDay (m LocalDate) where
  fromYearDay = fromYearDay'

fromYearDay' :: MonadFail m => Integer -> Int -> m LocalDate
fromYearDay' year day =
  if day < 1 || day > getDaysInYear year
    then fail $ "Invalid day of year: " ++ show day
    else pure . fromEpochDay $
         getEpochDay (fromJust (fromDate year 1 1) :: LocalDate) +
         fromIntegral (day - 1)

instance MonadFail m => FromDate (m LocalDate) where
  fromDate = fromDate'

fromDate' :: MonadFail m => Integer -> Int -> Int -> m LocalDate
fromDate' year month day =
  if monthIsInvalid || dayIsInvalid
    then fail errMsg
    else pure $ LocalDate year (fromIntegral month) (fromIntegral day)
  where
    monthIsInvalid = month < 1 || month > 12
    dayIsInvalid =
      day < 1 || day > getDaysInMonth (isLeapYear year) (toEnum month)
    errMsg =
      "Invalid date: year=" ++
      show year ++ ", month=" ++ show month ++ ", dayOfMonth=" ++ show day

clipDayOfMonth :: Integer -> Month -> Int -> LocalDate
clipDayOfMonth year month day = fromJust $ fromDate year (fromEnum month) day'
  where
    day' =
      let maxDay = getDaysInMonth (isLeapYear year) month
       in if maxDay > day
            then day
            else maxDay

instance HasYear LocalDate where
  getYear (LocalDate y _ _) = y
  setYear = setYear'

setYear' :: Integer -> LocalDate -> LocalDate
setYear' y date
  | y == getYear date = date
  | otherwise = clipDayOfMonth y (getMonth date) (getDayOfMonth date)

instance HasMonth LocalDate where
  getMonth (LocalDate _ m _) = toEnum $ fromIntegral m
  addMonths = addMonths'
  setMonth = setMonth'

addMonths' :: Int -> LocalDate -> LocalDate
addMonths' 0 date = date
addMonths' n date = clipDayOfMonth year month (getDayOfMonth date)
  where
    months =
      getYear date * 12 + fromIntegral (fromEnum $ getMonth date) +
      fromIntegral n -
      1
    year = months `div` 12
    month = toEnum . fromIntegral $ months `mod` 12 + 1

setMonth' :: Month -> LocalDate -> LocalDate
setMonth' month date
  | month == getMonth date = date
  | otherwise = clipDayOfMonth (getYear date) month (getDayOfMonth date)

instance HasDayOfMonth LocalDate where
  getDayOfMonth (LocalDate _ _ d) = fromIntegral d
  addDays = addDays'

instance HasDayOfYear LocalDate where
  getDayOfYear = getDayOfYear'

addDays' :: Int -> LocalDate -> LocalDate
addDays' 0 date = date
addDays' days date = fromEpochDay $ getEpochDay date + fromIntegral days

getDayOfYear' :: LocalDate -> Int
getDayOfYear' date = getFirstDayOfYear leap month + day - 1
  where
    leap = isLeapYear $ getYear date
    month = getMonth date
    day = getDayOfMonth date

instance HasEpochDay LocalDate where
  getEpochDay (LocalDate y month day)
        -- Ported from java.time.LocalDate.getEpochDay
   =
    365 * y +
    (if y >= 0
       then (y + 3) `div` 4 - (y + 99) `div` 100 + (y + 399) `div` 400
       else -(y `div` (-4) - y `div` (-100) + y `div` (-400))) +
    ((367 * fromIntegral month - 362) `div` 12) +
    (fromIntegral day - 1) +
    (if month <= 2
       then 0
       else if isLeapYear y
              then (-1)
              else (-2)) -
    numDaysFromYear0To1970
    where
      numDaysFromYear0To1970 = 719528

instance Show LocalDate where
  show = show'

show' :: LocalDate -> String
show' date = year ++ "-" ++ month ++ "-" ++ day
  where
    yearAbs = abs $ getYear date
    year =
      (if getYear date < 0
         then "-"
         else "") ++
      pad 4 (show yearAbs)
    month = pad 2 . show . fromEnum . getMonth $ date
    day = pad 2 . show . getDayOfMonth $ date
    pad n s =
      let len = length s
       in if len >= n
            then s
            else replicate (n - len) '0' ++ s
