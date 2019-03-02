module Data.Time
  ( Instant(..)
  , epoch
  , fromEpochMilli
  , toEpochMilli
  , isLeapYear
  , Month(..)
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

-- | A year is a leap year if it is divisible by 4
-- but not divisible by 100 unless it's divisible by 400.
isLeapYear :: Integer -> Bool
isLeapYear year =
  year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Ord, Bounded, Show)

-- | January -> 1, February -> 2, etc.
instance Enum Month where

  fromEnum month = case month of
    January   -> 1
    February  -> 2
    March     -> 3
    April     -> 4
    May       -> 5
    June      -> 6
    July      -> 7
    August    -> 8
    September -> 9
    October   -> 10
    November  -> 11
    December  -> 12

  toEnum i = case i of
    1  -> January
    2  -> February
    3  -> March
    4  -> April
    5  -> May
    6  -> June
    7  -> July
    8  -> August
    9  -> September
    10 -> October
    11 -> November
    12 -> December
    _  -> error ("Uknown month: " ++ show i)
