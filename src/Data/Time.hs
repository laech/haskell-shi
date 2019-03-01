module Data.Time
  ( Instant(..)
  , toEpochMillis
  )
where

data Instant = Instant
  { getEpochSecond :: Integer
  , getNano :: Int
  } deriving (Eq, Ord, Show)

-- | 1970-01-01T00:00:00Z
epoch :: Instant
epoch = Instant 0 0

-- | Converts an instant to number of milliseconds since 'epoch'.
toEpochMillis :: Instant -> Integer
toEpochMillis (Instant sec nano) =
  sec * 1000 + fromIntegral (nano `div` 1000000)
