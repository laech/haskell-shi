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
          [getDaysInMonth False month, getDaysInMonth True month]
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
localDateSpec = describe "LocalDate" $ do

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

  describe "toEpochDay" $ do

    it "returns 0 for epoch" (toEpochDay (LocalDate 1970 1 1) `shouldBe` 0)

    mapM_
      (\(localDate, epochDay) -> it
        ("returns correct values for " ++ show localDate)
        (toEpochDay localDate `shouldBe` epochDay)
      )
      [ (LocalDate 1970 1 2, 1)
      , (LocalDate 1970 1 3, 2)
      , (LocalDate 1970 12 31, 364)
      , (LocalDate 1971 1 1, 365)
      , (LocalDate 2004 2 29, 12477)
      , (LocalDate 2019 3 17, 17972)
      , (LocalDate 999999999 12 31, 365241780471)
      , (LocalDate 74556927 9 2, 27230639126)
      , (LocalDate 941048034 12 12, 343710017376)
      , (LocalDate 56483748 1 8, 20629545808)
      , (LocalDate 243946412 7 19, 89098878057)
      , (LocalDate 78675278 5 11, 28734835828)
      , (LocalDate 32078367 12 25, 11715663789)
      , (LocalDate 463697857 4 1, 169361445098)
      , (LocalDate 443805972 8 24, 162096083436)
      , (LocalDate 470418915 7 16, 171816261230)
      , (LocalDate 897678790 5 23, 327869726071)
      , (LocalDate 162193233 1 15, 59239142391)
      , (LocalDate 534365546 5 28, 195172288554)
      , (LocalDate 1969 12 31, -1)
      , (LocalDate 1969 12 30, -2)
      , (LocalDate 1969 1 1, -365)
      , (LocalDate 1960 2 29, -3594)
      , (LocalDate 0 1 1, -719528)
      , (LocalDate 0 12 31, -719163)
      , (LocalDate (-400) 12 31, -865260)
      , (LocalDate (-1) 12 31, -719529)
      , (LocalDate (-1) 1 1, -719893)
      , (LocalDate (-999999999) 1 1, -365243219162)
      , (LocalDate (-44758093) 11 19, -16348276989)
      , (LocalDate (-302472524) 10 20, -110476540082)
      , (LocalDate (-984111839) 12 8, -359440187542)
      , (LocalDate (-750556618) 8 8, -274135894858)
      , (LocalDate (-882146331) 9 3, -322198050582)
      , (LocalDate (-785520990) 2 4, -286906369684)
      , (LocalDate (-26911170) 6 23, -9829822363)
      , (LocalDate (-296189792) 6 13, -108181819469)
      ]

  describe "localDateOf" $ do

    it "fails on invalid dates" $ do
      localDateOf 1970 2 29 `shouldBe` Nothing
      localDateOf 1970 0 1 `shouldBe` Nothing
      localDateOf 1970 1 0 `shouldBe` Nothing
      localDateOf 2000 1 90 `shouldBe` Nothing
      localDateOf 2011 (-1) 1 `shouldBe` Nothing
      localDateOf 2018 1 (-3) `shouldBe` Nothing

    it "returns valid dates" $ do
      localDateOf 1970 1 1 `shouldBe` Just (LocalDate 1970 1 1)
      localDateOf 2000 2 29 `shouldBe` Just (LocalDate 2000 2 29)
      localDateOf (-1) 1 31 `shouldBe` Just (LocalDate (-1) 1 31)
      localDateOf 99999 12 31 `shouldBe` Just (LocalDate 99999 12 31)
