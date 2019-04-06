module Data.Time.MonthSpec where

import Data.List (sort)
import Data.Time.Month
import Test.Hspec

spec :: Spec
spec =
  describe "Month" $ do
    monthEnumSpec
    monthOfSpec
    monthSortSpec
    describe "getDaysInMonth" getDaysInMonthSpec

-- | Months in the order we expect
months :: [Month]
months =
  [ January
  , February
  , March
  , April
  , May
  , June
  , July
  , August
  , September
  , October
  , November
  , December
  ]

monthEnumSpec :: Spec
monthEnumSpec =
  it "produces natural month numbers" $ map fromEnum months `shouldBe` [1 .. 12]

monthOfSpec :: Spec
monthOfSpec =
  it "creates from natural month numbers" $
  mapM monthOf [1 .. 12] `shouldBe` Just months

monthSortSpec :: Spec
monthSortSpec =
  it "sorts naturally" $ do
    sort months `shouldBe` months
    [minBound .. maxBound] `shouldBe` months

getDaysInMonthSpec :: Spec
getDaysInMonthSpec = do
  it "January has 31 days" (expectDaysInMonth January 31 31)
  it "February has 28/29 days" (expectDaysInMonth February 28 29)
  it "March has 31 days" (expectDaysInMonth March 31 31)
  it "April has 30 days" (expectDaysInMonth April 30 30)
  it "May has 31 days" (expectDaysInMonth May 31 31)
  it "June has 30 days" (expectDaysInMonth June 30 30)
  it "July has 31 days" (expectDaysInMonth July 31 31)
  it "August has 31 days" (expectDaysInMonth August 31 31)
  it "September has 30 days" (expectDaysInMonth September 30 30)
  it "October has 31 days" (expectDaysInMonth October 31 31)
  it "November has 30 days" (expectDaysInMonth November 30 30)
  it "December has 31 days" (expectDaysInMonth December 31 31)
  where
    expectDaysInMonth month daysInNonLeapYear daysInLeapYear =
      [getDaysInMonth False month, getDaysInMonth True month] `shouldBe`
      [daysInNonLeapYear, daysInLeapYear]
