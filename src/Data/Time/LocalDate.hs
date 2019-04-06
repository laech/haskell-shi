module Data.Time.LocalDate
  ( LocalDate
  , localDateOf
  , module Data.Time.Base
  ) where

import Control.Monad.Fail
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
  deriving (Eq, Ord, Show)

-- | Creates a local date from year, month, day. Errors if date is invalid.
localDateOf :: MonadFail m => Integer -> Int -> Int -> m LocalDate
localDateOf year month day =
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

instance HasYear LocalDate where
  getYear (LocalDate y _ _) = y

instance HasMonth LocalDate where
  getMonth (LocalDate _ m _) = fromIntegral m

instance HasDayOfMonth LocalDate where
  getDayOfMonth (LocalDate _ _ d) = fromIntegral d

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
