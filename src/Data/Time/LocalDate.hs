module Data.Time.LocalDate
  ( LocalDate
  , getYear
  , getMonth
  , getDayOfMonth
  , getEpochDay
  , localDateOf
  ) where

import Control.Monad.Fail
import Data.Time.Base
import Data.Word
import Prelude hiding (fail)

-- | A date without time zone.
data LocalDate =
  LocalDate Integer
            Word8
            Word8
  deriving (Eq, Ord, Show)

getYear :: LocalDate -> Integer
getYear (LocalDate y _ _) = y

getMonth :: LocalDate -> Int
getMonth (LocalDate _ m _) = fromIntegral m

getDayOfMonth :: LocalDate -> Int
getDayOfMonth (LocalDate _ _ d) = fromIntegral d

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

-- | The day count since epoch, where day 0 is 1970-01-01.
getEpochDay :: LocalDate -> Integer
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
