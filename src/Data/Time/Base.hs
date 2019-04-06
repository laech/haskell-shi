module Data.Time.Base
  ( HasYear(..)
  , HasMonth(..)
  , HasDayOfMonth(..)
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

class HasMonth a where
  getMonth :: a -> Int

class HasDayOfMonth a where
  getDayOfMonth :: a -> Int

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
  -- | Get the the time as seconds since the start of the day.
  getSecondOfDay :: a -> Int

class HasEpochDay a where
  -- | The day count since epoch, where day 0 is 1970-01-01.
  getEpochDay :: a -> Integer

class HasEpochSecond a where
  -- | Gets the time as number of seconds since epoch 1970-01-01T00:00:00Z.
  getEpochSecond :: a -> Integer

class HasEpochMilli a where
  -- | Gets the time as number of milliseconds since epoch 1970-01-01T00:00:00Z.
  getEpochMilli :: a -> Integer
