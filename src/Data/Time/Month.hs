module Data.Time.Month
  ( Month(..)
  , monthOf
  , getDaysInMonth
  ) where

import Control.Monad.Fail
import Data.Maybe
import Prelude hiding (fail)

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
  fromEnum = monthFromEnum
  toEnum = monthToEnum

monthFromEnum :: Month -> Int
monthFromEnum month =
  case month of
    January -> 1
    February -> 2
    March -> 3
    April -> 4
    May -> 5
    June -> 6
    July -> 7
    August -> 8
    September -> 9
    October -> 10
    November -> 11
    December -> 12

monthToEnum :: Int -> Month
monthToEnum i = fromMaybe (error $ "Invalid month: " ++ show i) (monthOf i)

-- | Converts a numeric month to a 'Month',
-- fails if given value is not between 1..12.
monthOf :: MonadFail m => Int -> m Month
monthOf i =
  case i of
    1 -> pure January
    2 -> pure February
    3 -> pure March
    4 -> pure April
    5 -> pure May
    6 -> pure June
    7 -> pure July
    8 -> pure August
    9 -> pure September
    10 -> pure October
    11 -> pure November
    12 -> pure December
    _ -> fail ("Invalid month: " ++ show i)

-- | Gets the number of days in a month.
getDaysInMonth ::
     Bool -- ^ Is leap year?
  -> Month
  -> Int
getDaysInMonth True February = 29
getDaysInMonth _ month =
  case month of
    January -> 31
    February -> 28
    March -> 31
    April -> 30
    May -> 31
    June -> 30
    July -> 31
    August -> 31
    September -> 30
    October -> 31
    November -> 30
    December -> 31
