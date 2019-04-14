module Data.Time.Base
  ( HasYear(..)
  , HasMonth(..)
  , HasDayOfMonth(..)
  , HasDayOfYear(..)
  , HasHour(..)
  , HasMinute(..)
  , HasSecond(..)
  , HasNanoOfSecond(..)
  , HasSecondOfDay(..)
  , HasEpochDay(..)
  , HasEpochSecond(..)
  , HasEpochMilli(..)
  ) where

class HasYear a where
  getYear :: a -> Integer

  -- | Adds the given number of years, can be negative.  The
  -- resulting day of month will be adjusted if any, for example,
  -- adding 1 year to 2000-02-29 will return 2001-02-28 as 2001-02-29
  -- would be invalid.
  addYears :: Int -> a -> a

class HasMonth a where
  getMonth :: a -> Int

  -- | Adds the given number of months, can be negative.  The
  -- resulting day of month will be adjusted if any, for example,
  -- adding 1 month to 2000-10-31 will return 2000-11-30 as 2000-11-31
  -- would be invalid.
  addMonths :: Int -> a -> a

class HasDayOfMonth a where
  getDayOfMonth :: a -> Int

class HasDayOfYear a where

  -- | The day of year, between 1 and 365 (or 366 for a leap year).
  getDayOfYear :: a -> Int

  -- | Adds the given number of days, can be negative. Increments or
  -- decrements the month and year fields etc if any.
  addDays :: Int -> a -> a

class HasHour a where
  getHour :: a -> Int

class HasMinute a where
  getMinute :: a -> Int

class HasSecond a where
  getSecond :: a -> Int

class HasNanoOfSecond a where
  -- | For a time value with nano second precision, return the number
  -- of nano seconds (between 0 - 999,999,999) from the whole second
  -- time value.
  getNanoOfSecond :: a -> Int

class HasSecondOfDay a where
  -- | The second since the start of the day.
  getSecondOfDay :: a -> Int

class HasEpochDay a where
  -- | The day since epoch, where day 0 is 1970-01-01.
  getEpochDay :: a -> Integer

class HasEpochSecond a where
  -- | The second since epoch 1970-01-01T00:00:00Z.
  getEpochSecond :: a -> Integer

class HasEpochMilli a where
  -- | The millisecond since epoch 1970-01-01T00:00:00Z.
  getEpochMilli :: a -> Integer
