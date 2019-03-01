module Data.Time
  ( Instant(..)
  , epoch
  , toEpochMilli
  )
where

data Instant = Instant
  { getEpochSecond :: Integer
  , getNano :: Int
  } deriving (Eq, Ord, Show)

-- | 1970-01-01T00:00:00Z
epoch :: Instant
epoch = Instant 0 0

-- | Converts an instant to the millisecond since 'epoch'.
toEpochMilli :: Instant -> Integer
toEpochMilli (Instant sec ns) = sec * 1000 + fromIntegral (ns `div` 1000000)
