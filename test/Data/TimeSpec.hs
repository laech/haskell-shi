module Data.TimeSpec where

import           Data.List                      ( sort )
import           Data.Time
import           Test.Hspec

spec :: Spec
spec = do
  instantSpec
  yearSpec
  monthSpec
  localDateSpec

instantSpec :: Spec
instantSpec = describe "Instant" $ do

  describe "compare" $ do

    it "returns EQ when values equal"
       ((Instant 1 2 `compare` Instant 1 2) `shouldBe` EQ)

    it "returns LT when value is less" $ do
      (Instant 1 2 `compare` Instant 1 3) `shouldBe` LT
      (Instant 1 2 `compare` Instant 2 2) `shouldBe` LT

    it "returns GT when value is greater" $ do
      (Instant 1 3 `compare` Instant 1 2) `shouldBe` GT
      (Instant 2 2 `compare` Instant 1 2) `shouldBe` GT

  describe "fromEpochMilli" $ do

    it "returns epoch for 0" $ fromEpochMilli 0 `shouldBe` epoch

    it "returns instant after epoch for positive value" $ do
      fromEpochMilli 1 `shouldBe` Instant 0 1000000
      fromEpochMilli 123456 `shouldBe` Instant 123 456000000

    it "returns instant before epoch for negative value" $ do
      fromEpochMilli (-1) `shouldBe` Instant (-1) 999000000
      fromEpochMilli (-10200) `shouldBe` Instant (-11) 800000000

  describe "toEpochMilli" $ do

    it "returns 0 for epoch time" $ toEpochMilli (Instant 0 0) `shouldBe` 0

    it "returns correct value for time after epoch" $ do
      toEpochMilli (Instant 1 2) `shouldBe` 1000
      toEpochMilli (Instant 11 200000000) `shouldBe` 11200

    it "returns correct value for time before epoch" $ do
      toEpochMilli (Instant (-1) 2) `shouldBe` (-1000)
      toEpochMilli (Instant (-1) 200000000) `shouldBe` (-800)

yearSpec :: Spec
yearSpec = describe "Year" $ do

  describe "isLeapYear" $ do
    it "if it's divisible by 4" $ isLeapYear 4 `shouldBe` True
    it "but not divisible by 100" $ isLeapYear 1700 `shouldBe` False
    it "unless it's divisible by 400" $ isLeapYear 1600 `shouldBe` True
    it "false otherwise" $ isLeapYear 2019 `shouldBe` False

  describe "getDaysInYear" $ do
    it "returns 366 for a leap year" $ getDaysInYear 1600 `shouldBe` 366
    it "returns 365 for a non leap year" $ getDaysInYear 2011 `shouldBe` 365

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

monthSpec :: Spec
monthSpec = describe "Month" $ do

  it "produces natural month numbers" (map fromEnum months `shouldBe` [1 .. 12])

  it "creates from natural month numbers"
     (mapM monthOf [1 .. 12] `shouldBe` Just months)

  it "sorts naturally" $ do
    sort months `shouldBe` months
    [minBound .. maxBound] `shouldBe` months

  describe "getDaysInMonth" $ do

    let expectDaysInMonth month daysInNonLeapYear daysInLeapYear =
          map (getDaysInMonth month) [False, True]
            `shouldBe` [daysInNonLeapYear, daysInLeapYear]

    it "January has 31 days"     (expectDaysInMonth January 31 31)
    it "February has 28/29 days" (expectDaysInMonth February 28 29)
    it "March has 31 days"       (expectDaysInMonth March 31 31)
    it "April has 30 days"       (expectDaysInMonth April 30 30)
    it "May has 31 days"         (expectDaysInMonth May 31 31)
    it "June has 30 days"        (expectDaysInMonth June 30 30)
    it "July has 31 days"        (expectDaysInMonth July 31 31)
    it "August has 31 days"      (expectDaysInMonth August 31 31)
    it "September has 30 days"   (expectDaysInMonth September 30 30)
    it "October has 31 days"     (expectDaysInMonth October 31 31)
    it "November has 30 days"    (expectDaysInMonth November 30 30)
    it "December has 31 days"    (expectDaysInMonth December 31 31)

localDateSpec :: Spec
localDateSpec = describe "LocalDate" $

  describe "compare" $ do

    it "returns EQ when values equal"
       ((LocalDate 1 2 3 `compare` LocalDate 1 2 3) `shouldBe` EQ)

    it "returns LT when year is less"
       ((LocalDate 1 2 3 `compare` LocalDate 2 2 3) `shouldBe` LT)

    it "returns LT when month is less"
       ((LocalDate 1 2 3 `compare` LocalDate 1 3 3) `shouldBe` LT)

    it "returns LT when day is less"
       ((LocalDate 1 2 3 `compare` LocalDate 1 2 4) `shouldBe` LT)

    it "returns GT when year is greater"
       ((LocalDate 2 2 3 `compare` LocalDate 1 2 3) `shouldBe` GT)

    it "returns GT when month is greater"
       ((LocalDate 1 3 3 `compare` LocalDate 1 2 3) `shouldBe` GT)

    it "returns GT when day is greater"
       ((LocalDate 1 2 4 `compare` LocalDate 1 2 3) `shouldBe` GT)
