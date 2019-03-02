module Data.Time
  ( Instant(..)
  , epoch
  , fromEpochMilli
  , toEpochMilli
  , isLeapYear
  , Month(..)
  , monthOf
  , getDaysInMonth
  )
where

import           Data.Maybe
import           Prelude                 hiding ( fail )
import           Control.Monad.Fail

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

-- | January is 1, February is 2, etc.
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

  toEnum i = fromMaybe (error $ "Uknown month: " ++ show i) (monthOf i)

-- | Converts a numeric month to a 'Month', 
-- fails if given value is not between 1..12.
monthOf :: MonadFail m => Int -> m Month
monthOf i = case i of
  1  -> pure January
  2  -> pure February
  3  -> pure March
  4  -> pure April
  5  -> pure May
  6  -> pure June
  7  -> pure July
  8  -> pure August
  9  -> pure September
  10 -> pure October
  11 -> pure November
  12 -> pure December
  _  -> fail ("Uknown month: " ++ show i)

-- | Gets the number of days in a month.
getDaysInMonth
  :: Month
  -> Bool -- ^ Is leap year?
  -> Int
getDaysInMonth month leapYear = case month of
  January   -> 31
  February  -> if leapYear then 29 else 28
  March     -> 31
  April     -> 30
  May       -> 31
  June      -> 30
  July      -> 31
  August    -> 31
  September -> 30
  October   -> 31
  November  -> 30
  December  -> 31
