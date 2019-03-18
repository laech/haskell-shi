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

    it "returns 0 for epoch" 
      (toEpochDay (LocalDate 1970 1 1) `shouldBe` 0)

    it "returns correct values for positive days" $ do
      toEpochDay (LocalDate 1970 1 2) `shouldBe` 1
      toEpochDay (LocalDate 1970 1 3) `shouldBe` 2
      toEpochDay (LocalDate 1970 12 31) `shouldBe` 364
      toEpochDay (LocalDate 1971 1 1) `shouldBe` 365
      toEpochDay (LocalDate 2004 2 29) `shouldBe` 12477
      toEpochDay (LocalDate 2019 3 17) `shouldBe` 17972
      toEpochDay (LocalDate 999999999 12 31) `shouldBe` 365241780471
      toEpochDay (LocalDate 74556927 9 2) `shouldBe` 27230639126
      toEpochDay (LocalDate 941048034 12 12) `shouldBe` 343710017376
      toEpochDay (LocalDate 56483748 1 8) `shouldBe` 20629545808
      toEpochDay (LocalDate 243946412 7 19) `shouldBe` 89098878057
      toEpochDay (LocalDate 78675278 5 11) `shouldBe` 28734835828
      toEpochDay (LocalDate 32078367 12 25) `shouldBe` 11715663789
      toEpochDay (LocalDate 463697857 4 1) `shouldBe` 169361445098
      toEpochDay (LocalDate 443805972 8 24) `shouldBe` 162096083436
      toEpochDay (LocalDate 470418915 7 16) `shouldBe` 171816261230
      toEpochDay (LocalDate 897678790 5 23) `shouldBe` 327869726071
      toEpochDay (LocalDate 162193233 1 15) `shouldBe` 59239142391
      toEpochDay (LocalDate 534365546 5 28) `shouldBe` 195172288554

    it "returns correct values for negative days" $ do
      toEpochDay (LocalDate 1969 12 31) `shouldBe` (-1)
      toEpochDay (LocalDate 1969 12 30) `shouldBe` (-2)
      toEpochDay (LocalDate 1969 1 1) `shouldBe` (-365)
      toEpochDay (LocalDate 1960 2 29) `shouldBe` (-3594)
      toEpochDay (LocalDate 0 1 1) `shouldBe` (-719528)
      toEpochDay (LocalDate 0 12 31) `shouldBe` (-719163)
      toEpochDay (LocalDate (-400) 12 31) `shouldBe` (-865260)
      toEpochDay (LocalDate (-1) 12 31) `shouldBe` (-719529)
      toEpochDay (LocalDate (-1) 1 1) `shouldBe` (-719893)
      toEpochDay (LocalDate (-999999999) 1 1) `shouldBe` (-365243219162)
      toEpochDay (LocalDate (-44758093) 11 19) `shouldBe` (-16348276989)
      toEpochDay (LocalDate (-302472524) 10 20) `shouldBe` (-110476540082)
      toEpochDay (LocalDate (-984111839) 12 8) `shouldBe` (-359440187542)
      toEpochDay (LocalDate (-750556618) 8 8) `shouldBe` (-274135894858)
      toEpochDay (LocalDate (-882146331) 9 3) `shouldBe` (-322198050582)
      toEpochDay (LocalDate (-785520990) 2 4) `shouldBe` (-286906369684)
      toEpochDay (LocalDate (-26911170) 6 23) `shouldBe` (-9829822363)
      toEpochDay (LocalDate (-296189792) 6 13) `shouldBe` (-108181819469)
