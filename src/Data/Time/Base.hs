module Data.Time
  ( HasEpochSecond(..)
  , HasNanoOfSecond(..)
  ) where

class HasEpochSecond a where
  -- | Gets the time as number of seconds since epoch.
  getEpochSecond :: a -> Integer

class HasNanoOfSecond a where
  -- | For a time value with nano second precision, return the number
  -- of nano seconds (between 0 - 999,999,999) from the whole second
  -- time value.
  getNanoOfSecond :: a -> Int
