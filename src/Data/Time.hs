module Data.Time
  ( Instant(..)
  , toEpochMillis
  )
where

data Instant = Instant
  { getEpochSecond :: Integer
  , getNano :: Int
  } deriving (Eq, Ord, Show)

toEpochMillis :: Instant -> Integer
toEpochMillis (Instant sec nano)
  | sec >= 0  = sec * 1000 + fromIntegral (nano `div` 1000000)
  | otherwise = sec * 1000 + fromIntegral (nano `div` 1000000) -- TODO
