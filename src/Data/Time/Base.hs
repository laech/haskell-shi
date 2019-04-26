module Data.Time.Base
  ( HasYear(..)
  , modifyYear
  , addYears
  , HasMonth(..)
  , HasDayOfMonth(..)
  , HasDayOfYear(..)
  , HasHour(..)
  , HasMinute(..)
  , HasSecond(..)
  , HasNanoOfSecond(..)
  , HasNanoOfDay(..)
  , HasSecondOfDay(..)
  , HasEpochDay(..)
  , HasEpochSecond(..)
  , HasEpochMilli(..)
  ) where

import Data.Time.Month

class HasYear a where
  getYear :: a -> Integer

  -- | Sets the year field. The resulting day of month will be
  -- adjusted if any, for example, setting the year from 2000-02-29 to
  -- 2001 will return 2001-02-28, as 2001-02-29 would be invalid.
  setYear :: Integer -> a -> a

-- | Calls 'setYear' with the new year by apply the old year to a
-- function.
modifyYear :: HasYear a => (Integer -> Integer) -> a -> a
modifyYear f a = setYear (f $ getYear a) a

-- | Calls 'setYear' by adding a value to the old year.
addYears :: HasYear a => Int -> a -> a
addYears n = modifyYear (+ fromIntegral n)

class HasMonth a where
  getMonth :: a -> Month

  -- | Adds the given number of months, can be negative. The resulting
  -- day of month will be adjusted if any, for example, adding 1 month
  -- to 2000-10-31 will return 2000-11-30 as 2000-11-31 would be
  -- invalid.
  addMonths :: Int -> a -> a

  -- | Sets the month field. The resulting day of month will be
  -- adjusted if any, for example, setting the month of 2000-10-31 to
  -- 11 will return 2000-11-30, as 2000-11-31 would be invalid.
  setMonth :: Month -> a -> a

class HasDayOfMonth a where
  getDayOfMonth :: a -> Int

class HasDayOfYear a where

  -- | The day of year, between 1 and 365 (or 366 for a leap year).
  getDayOfYear :: a -> Int

class HasHour a where
  getHour :: a -> Int
  addHours :: Int -> a -> a

class HasMinute a where
  getMinute :: a -> Int
  addMinutes :: Int -> a -> a

class HasSecond a where
  getSecond :: a -> Int
  addSeconds :: Int -> a -> a

class HasNanoOfSecond a where
  -- | For a time value with nano second precision, return the number
  -- of nano seconds (between 0 - 999,999,999) from the whole second
  -- time value.
  getNanoOfSecond :: a -> Int
  addNanos :: Int -> a -> a

class HasNanoOfDay a where
  -- | Gets the time as the nanosecond since the start of the day.
  getNanoOfDay :: a -> Integer

class HasSecondOfDay a where
  -- | Gets the time as the second since the start of the day.
  getSecondOfDay :: a -> Int

class HasEpochDay a where
  -- | The day since epoch, where day 0 is 1970-01-01.
  getEpochDay :: a -> Integer

  -- | Adds the given number of days, can be negative. Increments or
  -- decrements the month and year fields etc if any.
  addDays :: Int -> a -> a

class HasEpochSecond a where
  -- | The second since epoch 1970-01-01T00:00:00Z.
  getEpochSecond :: a -> Integer

class HasEpochMilli a where
  -- | The millisecond since epoch 1970-01-01T00:00:00Z.
  getEpochMilli :: a -> Integer
