module Data.Time
  ( Instant(..)
  , epoch
  , fromEpochMilli
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

-- | Converts a millisecond from 'epoch' to an instant.
fromEpochMilli :: Integer -> Instant
fromEpochMilli ms = Instant sec ns
 where
  sec = ms `div` 1000
  ns  = fromIntegral (ms - sec * 1000) * 1000000

-- | Converts an instant to the millisecond since 'epoch'.
toEpochMilli :: Instant -> Integer
toEpochMilli (Instant sec ns) = sec * 1000 + fromIntegral (ns `div` 1000000)
