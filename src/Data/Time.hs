module Data.Time
  ( Instant
  , getEpochSecond
  , getNanoOfSecond
  , epoch
  , fromEpochSecond
  , fromEpochMilli
  , toEpochMilli
  , isLeapYear
  , getDaysInYear
  , Month(..)
  , monthOf
  , getDaysInMonth
  , LocalDate
  , getYear
  , getMonth
  , getDayOfMonth
  , localDateOf
  , toEpochDay
  ) where

import Control.Monad.Fail
import Data.Maybe
import Prelude hiding (fail)

data Instant =
  Instant Integer
          Int
  deriving (Eq, Ord, Show)

getEpochSecond :: Instant -> Integer
getEpochSecond (Instant s _) = s

getNanoOfSecond :: Instant -> Int
getNanoOfSecond (Instant _ n) = n

-- | 1970-01-01T00:00:00Z
epoch :: Instant
epoch = Instant 0 0

fromEpochSecond :: Integer -> Int -> Instant
fromEpochSecond = Instant -- TODO

-- | Converts a millisecond from 'epoch' to an instant.
fromEpochMilli :: Integer -> Instant
fromEpochMilli ms = Instant sec ns
  where
    sec = ms `div` 1000
    ns = fromIntegral (ms - sec * 1000) * 1000000

-- | Converts an instant to the millisecond since 'epoch'.
toEpochMilli :: Instant -> Integer
toEpochMilli (Instant sec ns) = sec * 1000 + fromIntegral (ns `div` 1000000)

-- | A year is a leap year if it is divisible by 4
-- but not divisible by 100 unless it's divisible by 400.
isLeapYear :: Integer -> Bool
isLeapYear year =
  year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)

-- | Gets the number of days in a year.
getDaysInYear :: Integer -> Int
getDaysInYear year =
  if isLeapYear year
    then 366
    else 365

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Ord, Bounded, Show)

-- | January is 1, February is 2, etc.
instance Enum Month where
  fromEnum = monthFromEnum
  toEnum = monthToEnum

monthFromEnum :: Month -> Int
monthFromEnum month =
  case month of
    January -> 1
    February -> 2
    March -> 3
    April -> 4
    May -> 5
    June -> 6
    July -> 7
    August -> 8
    September -> 9
    October -> 10
    November -> 11
    December -> 12

monthToEnum :: Int -> Month
monthToEnum i = fromMaybe (error $ "Invalid month: " ++ show i) (monthOf i)

-- | Converts a numeric month to a 'Month',
-- fails if given value is not between 1..12.
monthOf :: MonadFail m => Int -> m Month
monthOf i =
  case i of
    1 -> pure January
    2 -> pure February
    3 -> pure March
    4 -> pure April
    5 -> pure May
    6 -> pure June
    7 -> pure July
    8 -> pure August
    9 -> pure September
    10 -> pure October
    11 -> pure November
    12 -> pure December
    _ -> fail ("Invalid month: " ++ show i)

-- | Gets the number of days in a month.
getDaysInMonth ::
     Bool -- ^ Is leap year?
  -> Month
  -> Int
getDaysInMonth True February = 29
getDaysInMonth _ month =
  case month of
    January -> 31
    February -> 28
    March -> 31
    April -> 30
    May -> 31
    June -> 30
    July -> 31
    August -> 31
    September -> 30
    October -> 31
    November -> 30
    December -> 31

-- | A date without time zone.
data LocalDate =
  LocalDate Integer
            Int
            Int
  deriving (Eq, Ord, Show)

getYear :: LocalDate -> Integer
getYear (LocalDate y _ _) = y

getMonth :: LocalDate -> Int
getMonth (LocalDate _ m _) = m

getDayOfMonth :: LocalDate -> Int
getDayOfMonth (LocalDate _ _ d) = d

-- | Creates a local date from year, month, day. Errors if date is invalid.
localDateOf :: MonadFail m => Integer -> Int -> Int -> m LocalDate
localDateOf year month day =
  if monthIsInvalid || dayIsInvalid
    then fail errMsg
    else pure $ LocalDate year month day
  where
    monthIsInvalid = month < 1 || month > 12
    dayIsInvalid =
      day < 1 || day > getDaysInMonth (isLeapYear year) (toEnum month)
    errMsg =
      "Invalid date: year=" ++
      show year ++ ", month=" ++ show month ++ ", dayOfMonth=" ++ show day

-- | The day count since epoch, where day 0 is 1970-01-01.
toEpochDay :: LocalDate -> Integer
toEpochDay (LocalDate y month day)
  -- Ported from java.time.LocalDate.toEpochDay
 =
  365 * y +
  (if y >= 0
     then (y + 3) `div` 4 - (y + 99) `div` 100 + (y + 399) `div` 400
     else -(y `div` (-4) - y `div` (-100) + y `div` (-400))) +
  fromIntegral ((367 * month - 362) `div` 12) +
  fromIntegral (day - 1) +
  (if month <= 2
     then 0
     else if isLeapYear y
            then (-1)
            else (-2)) -
  numDaysFromYear0To1970
  where
    numDaysFromYear0To1970 = 719528
