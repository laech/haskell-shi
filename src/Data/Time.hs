module Data.Time
  ( Instant(..)
  , toEpochMillis
  )
where

data Instant = Instant
  { getEpochSecond :: Integer
  , getNano :: Int
  } deriving (Eq, Ord, Show)

-- | Converts an instant to number of milliseconds since 'epoch'.
toEpochMillis :: Instant -> Integer
toEpochMillis (Instant sec nano) =
  sec * 1000 + fromIntegral (nano `div` 1000000)
