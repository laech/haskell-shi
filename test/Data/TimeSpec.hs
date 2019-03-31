module Data.TimeSpec where

import Data.List (sort)
import Data.Maybe
import Data.Time
import Test.Hspec

spec :: Spec
spec = do
  instantSpec
  yearSpec
  monthSpec
  localDateSpec

instantSpec :: Spec
instantSpec =
  describe "Instant" $ do
    describe "compare" instantCompareSpec
    describe "instantOfEpocMilli" instantOfEpochMilliSpec
    describe "getEpochMilli" getEpochMilliSpec

instantCompareSpec :: Spec
instantCompareSpec =
  mapM_
    test
    [ (EQ, instantOfEpochSecond 1 2, instantOfEpochSecond 1 2)
    , (LT, instantOfEpochSecond 1 2, instantOfEpochSecond 1 3)
    , (LT, instantOfEpochSecond 1 2, instantOfEpochSecond 2 2)
    , (GT, instantOfEpochSecond 1 3, instantOfEpochSecond 1 2)
    , (GT, instantOfEpochSecond 2 2, instantOfEpochSecond 1 2)
    ]
  where
    test arg@(expect, a, b) = it (show arg) $ a `compare` b `shouldBe` expect

instantOfEpochMilliSpec :: Spec
instantOfEpochMilliSpec = do
  mapM_
    test
    [ (0, epoch)
    , (1, instantOfEpochSecond 0 1000000)
    , (123456, instantOfEpochSecond 123 456000000)
    , (-1, instantOfEpochSecond (-1) 999000000)
    , (-10200, instantOfEpochSecond (-11) 800000000)
    ]
  where
    test arg@(milli, instant) =
      it (show arg) $ instantOfEpochMilli milli `shouldBe` instant

getEpochMilliSpec :: Spec
getEpochMilliSpec = do
  mapM_
    test
    [ (instantOfEpochSecond 0 0, 0)
    , (instantOfEpochSecond 1 2, 1000)
    , (instantOfEpochSecond 11 200000000, 11200)
    , (instantOfEpochSecond (-1) 2, -1000)
    , (instantOfEpochSecond (-1) 200000000, -800)
    ]
  where
    test arg@(instant, milli) =
      it (show arg) $ getEpochMilli instant `shouldBe` milli

yearSpec :: Spec
yearSpec =
  describe "Year" $ do
    describe "isLeapYear" isLeapYearSpec
    describe "getDaysInYear" getDaysInYearSpec

isLeapYearSpec :: Spec
isLeapYearSpec = do
  it "if it's divisible by 4" $ isLeapYear 4 `shouldBe` True
  it "but not divisible by 100" $ isLeapYear 1700 `shouldBe` False
  it "unless it's divisible by 400" $ isLeapYear 1600 `shouldBe` True
  it "false otherwise" $ isLeapYear 2019 `shouldBe` False

getDaysInYearSpec :: Spec
getDaysInYearSpec = do
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
monthSpec =
  describe "Month" $ do
    monthEnumSpec
    monthOfSpec
    monthSortSpec
    describe "getDaysInMonth" getDaysInMonthSpec

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

localDateSpec :: Spec
localDateSpec =
  describe "LocalDate" $ do
    describe "compare" localDateCompareSpec
    describe "getEpochDay" getEpochDaySpec
    describe "localDateOf" localDateOfSpec

localDateValid :: Integer -> Int -> Int -> LocalDate
localDateValid y m d = fromJust $ localDateOf y m d

localDateCompareSpec :: Spec
localDateCompareSpec =
  mapM_
    test
    [ (EQ, localDateValid 1 2 3, localDateValid 1 2 3)
    , (LT, localDateValid 1 2 3, localDateValid 2 2 3)
    , (LT, localDateValid 1 2 3, localDateValid 1 3 3)
    , (LT, localDateValid 1 2 3, localDateValid 1 2 4)
    , (GT, localDateValid 2 2 3, localDateValid 1 2 3)
    , (GT, localDateValid 1 3 3, localDateValid 1 2 3)
    , (GT, localDateValid 1 2 4, localDateValid 1 2 3)
    ]
  where
    test arg@(expect, a, b) = it (show arg) $ a `compare` b `shouldBe` expect

getEpochDaySpec :: Spec
getEpochDaySpec =
  mapM_
    test
    [ (localDateValid 1970 1 1, 0)
    , (localDateValid 1970 1 2, 1)
    , (localDateValid 1970 1 3, 2)
    , (localDateValid 1970 12 31, 364)
    , (localDateValid 1971 1 1, 365)
    , (localDateValid 2004 2 29, 12477)
    , (localDateValid 2019 3 17, 17972)
    , (localDateValid 999999999 12 31, 365241780471)
    , (localDateValid 74556927 9 2, 27230639126)
    , (localDateValid 941048034 12 12, 343710017376)
    , (localDateValid 56483748 1 8, 20629545808)
    , (localDateValid 243946412 7 19, 89098878057)
    , (localDateValid 78675278 5 11, 28734835828)
    , (localDateValid 32078367 12 25, 11715663789)
    , (localDateValid 463697857 4 1, 169361445098)
    , (localDateValid 443805972 8 24, 162096083436)
    , (localDateValid 470418915 7 16, 171816261230)
    , (localDateValid 897678790 5 23, 327869726071)
    , (localDateValid 162193233 1 15, 59239142391)
    , (localDateValid 534365546 5 28, 195172288554)
    , (localDateValid 1969 12 31, -1)
    , (localDateValid 1969 12 30, -2)
    , (localDateValid 1969 1 1, -365)
    , (localDateValid 1960 2 29, -3594)
    , (localDateValid 0 1 1, -719528)
    , (localDateValid 0 12 31, -719163)
    , (localDateValid (-400) 12 31, -865260)
    , (localDateValid (-1) 12 31, -719529)
    , (localDateValid (-1) 1 1, -719893)
    , (localDateValid (-999999999) 1 1, -365243219162)
    , (localDateValid (-44758093) 11 19, -16348276989)
    , (localDateValid (-302472524) 10 20, -110476540082)
    , (localDateValid (-984111839) 12 8, -359440187542)
    , (localDateValid (-750556618) 8 8, -274135894858)
    , (localDateValid (-882146331) 9 3, -322198050582)
    , (localDateValid (-785520990) 2 4, -286906369684)
    , (localDateValid (-26911170) 6 23, -9829822363)
    , (localDateValid (-296189792) 6 13, -108181819469)
    ]
  where
    test arg@(localDate, epochDay) =
      it (show arg) $ getEpochDay localDate `shouldBe` epochDay

localDateOfSpec :: Spec
localDateOfSpec =
  mapM_
    test
    [ (1970, 2, 29, Nothing)
    , (1970, 0, 1, Nothing)
    , (1970, 1, 0, Nothing)
    , (2000, 1, 90, Nothing)
    , (2011, -1, 1, Nothing)
    , (2018, 1, -3, Nothing)
    , (1970, 1, 1, Just (localDateValid 1970 1 1))
    , (2000, 2, 29, Just (localDateValid 2000 2 29))
    , (-1, 1, 31, Just (localDateValid (-1) 1 31))
    , (99999, 12, 31, Just (localDateValid 99999 12 31))
    ]
  where
    test arg@(y, m, d, expected) =
      it (show arg) $ (localDateOf y m d) `shouldBe` expected
