module Data.Time.Offset
  ( Offset(getTotalOffsetMinutes)
  , offsetOfMinutes
  , offsetOfHoursMinutes
  , utcOffset
  ) where

-- | A time offset from UTC.
newtype Offset = Offset
  { getTotalOffsetMinutes :: Int
  } deriving (Eq, Ord, Show)

offsetOfMinutes :: Int -> Offset
offsetOfMinutes = Offset

offsetOfHoursMinutes :: Int -> Int -> Offset
offsetOfHoursMinutes hours minutes = offsetOfMinutes (hours * 60 + minutes)

utcOffset :: Offset
utcOffset = offsetOfMinutes 0
