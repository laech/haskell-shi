module Data.Time
  ( Instant(..)
  )
where

data Instant = Instant
  { getEpochSecond :: Integer
  , getNano :: Int
  } deriving (Eq, Ord, Show)
