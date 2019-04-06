module Data.Time.Year
  ( isLeapYear
  , getDaysInYear
  ) where

-- | A year is a leap year if it is divisible by 4
-- but not divisible by 100 unless it's divisible by 400.
isLeapYear :: Integer -> Bool
isLeapYear year =
  year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)

-- | Gets the number of days in a year.
getDaysInYear :: Integer -> Int
getDaysInYear year =
  if isLeapYear year
    then 366
    else 365
