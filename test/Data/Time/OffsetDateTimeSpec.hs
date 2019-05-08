module Data.Time.OffsetDateTimeSpec where

import Data.Maybe
import Data.Time.Month
import Data.Time.OffsetDateTime
import Test.Hspec

spec :: Spec
spec =
  describe "OffsetDateTime" $ do
    describe "show" showSpec
    describe "getYear" getYearSpec
    describe "getMonth" getMonthSpec
    describe "getDayOfMonth" getDayOfMonthSpec
    describe "getDayOfYear" getDayOfYearSpec
    describe "getHour" getHourSpec
    describe "getMinute" getMinuteSpec
    describe "getSecond" getSecondSpec
    describe "getNanoOfSecond" getNanoOfSecondSpec
    describe "getLocalDate" getLocalDateSpec
    describe "getLocalTime" getLocalTimeSpec
    describe "getLocalDateTime" getLocalDateTimeSpec
    describe "getSecondOfDay" getSecondOfDaySpec
    describe "getNanoOfDay" getNanoOfDaySpec
    describe "getEpochDay" getEpochDaySpec
    describe "getEpochSecond" getEpochSecondSpec

--    describe "compare" compareSpec
--    describe "getEpochMilli" getEpochMilliSpec
--    describe "addNanos" addNanosSpec
--    describe "addSeconds" addSecondsSpec
--    describe "addMinutes" addMinutesSpec
--    describe "addHours" addHoursSpec
--    describe "addDays" addDaysSpec
--    describe "addMonths" addMonthsSpec
--    describe "addYears" addYearsSpec
--    describe "setYear" setYearSpec
--    describe "setMonth" setMonthSpec
--    describe "setLocalDate" setLocalDateSpec
--    describe "setLocalTime" setLocalTimeSpec
--    describe "setLocalDateTime" setLocalDateTimeSpec

offsetDateTime ::
     Integer
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Offset
  -> OffsetDateTime
offsetDateTime year month day hour minute second nano offset =
  fromJust $ fromDateTime year month day hour minute second nano offset

offsetOfSeconds' :: Int -> Offset
offsetOfSeconds' seconds = fromJust $ offsetOfSeconds seconds

showSpec :: Spec
showSpec =
  mapM_
    test
    [ ("1970-01-01T00:00Z", offsetDateTime 1970 1 1 0 0 0 0 utcOffset)
    , ( "1970-01-02T03:04:05+01:00"
      , offsetDateTime 1970 1 2 3 4 5 0 (offsetOfSeconds' 3600))
    , ( "1970-01-02T03:04:05-01:00"
      , offsetDateTime 1970 1 2 3 4 5 0 (offsetOfSeconds' (-3600)))
    ]
  where
    test :: (String, OffsetDateTime) -> Spec
    test (str, dt) = it (show str) $ show dt `shouldBe` str

getYearSpec :: Spec
getYearSpec =
  it "should return the correct year" $
  getYear (offsetDateTime 1970 1 1 2 3 4 5 utcOffset) `shouldBe` 1970

getMonthSpec :: Spec
getMonthSpec =
  it "should return the correct month" $
  getMonth (offsetDateTime 1970 12 1 2 3 4 5 utcOffset) `shouldBe` December

getDayOfMonthSpec :: Spec
getDayOfMonthSpec =
  it "should return the correct day of month" $
  getDayOfMonth (offsetDateTime 1970 1 9 2 3 4 5 utcOffset) `shouldBe` 9

getDayOfYearSpec :: Spec
getDayOfYearSpec =
  it "should return the correct day of year" $
  getDayOfYear (offsetDateTime 1970 12 31 2 3 4 5 utcOffset) `shouldBe` 365

getHourSpec :: Spec
getHourSpec =
  it "should return the correct hour" $
  getHour (offsetDateTime 1970 12 31 2 3 4 5 utcOffset) `shouldBe` 2

getMinuteSpec :: Spec
getMinuteSpec =
  it "should return the correct minute" $
  getMinute (offsetDateTime 1970 12 31 2 3 4 5 utcOffset) `shouldBe` 3

getSecondSpec :: Spec
getSecondSpec =
  it "should return the correct second" $
  getSecond (offsetDateTime 1970 12 31 2 3 4 5 utcOffset) `shouldBe` 4

getNanoOfSecondSpec :: Spec
getNanoOfSecondSpec =
  it "should return the correct nano of second" $
  getNanoOfSecond (offsetDateTime 1970 12 31 2 3 4 5 utcOffset) `shouldBe` 5

getLocalDateSpec :: Spec
getLocalDateSpec =
  it "should return the correct local date" $
  getLocalDate (offsetDateTime 1970 12 31 2 3 4 5 utcOffset) `shouldBe`
  fromJust (fromDate 1970 12 31)

getLocalTimeSpec :: Spec
getLocalTimeSpec =
  it "should return the correct local time" $
  getLocalTime (offsetDateTime 1970 12 31 2 3 4 5 utcOffset) `shouldBe`
  fromJust (fromTime 2 3 4 5)

getLocalDateTimeSpec :: Spec
getLocalDateTimeSpec =
  it "should return the correct local date time" $
  getLocalDateTime (offsetDateTime 1970 12 31 2 3 4 5 (offsetOfSeconds' 3600)) `shouldBe`
  fromJust (fromDateTime 1970 12 31 2 3 4 5)

getSecondOfDaySpec :: Spec
getSecondOfDaySpec =
  it "should return the correct second of day" $
  getSecondOfDay (offsetDateTime 1970 12 31 0 1 4 5 (offsetOfSeconds' 3600)) `shouldBe`
  64

getNanoOfDaySpec :: Spec
getNanoOfDaySpec =
  it "should return the correct nano of day" $
  getNanoOfDay (offsetDateTime 1970 12 31 0 1 4 5 (offsetOfSeconds' 3600)) `shouldBe`
  64000000005

getEpochDaySpec :: Spec
getEpochDaySpec =
  mapM_
    test
    [ (offsetDateTime 1970 1 1 0 0 0 0 utcOffset, 0)
    , (offsetDateTime 1970 1 1 12 0 0 0 utcOffset, 0)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' 43200), 0)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' (-43200)), 1)
    , (offsetDateTime 1970 1 2 0 0 0 0 utcOffset, 1)
    , (offsetDateTime 1970 1 1 23 59 59 0 utcOffset, 0)
    , (offsetDateTime 1970 1 1 23 59 59 0 (offsetOfSeconds' (-1)), 1)
    , (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 1), -1)
    ]
  where
    test arg@(datetime, day) =
      it (show arg) $ getEpochDay datetime `shouldBe` day

getEpochSecondSpec :: Spec
getEpochSecondSpec =
  mapM_
    test
    [ (offsetDateTime 1970 1 1 0 0 0 0 utcOffset, 0)
    , (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 1), -1)
    , (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' (-1)), 1)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' 43200), 0)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' 43199), 1)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' 43201), -1)
    , (offsetDateTime 1970 1 2 0 0 0 0 utcOffset, 86400)
    , (offsetDateTime 1970 1 1 23 59 59 0 utcOffset, 86399)
    , (offsetDateTime 1970 1 1 23 59 59 0 (offsetOfSeconds' (-1)), 86400)
    ]
  where
    test arg@(datetime, second) =
      it (show arg) $ getEpochSecond datetime `shouldBe` second

getEpochMilliSpec :: Spec
getEpochMilliSpec =
  mapM_
    test
    [ (offsetDateTime 1970 1 1 0 0 0 0 utcOffset, 0)
    , (offsetDateTime 1970 1 1 0 0 0 2000000 utcOffset, 2)
    , (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 1), -1)
    , (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' (-1)), 1)
    , (offsetDateTime 1970 1 1 12 0 0 1 (offsetOfSeconds' 43200), 0)
    , (offsetDateTime 1970 1 1 12 0 0 1000000 (offsetOfSeconds' 43199), 1001)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' 43201), -1000)
    , (offsetDateTime 1970 1 2 0 0 0 0 utcOffset, 86400000)
    , (offsetDateTime 1970 1 1 11 59 59 0 utcOffset, 86339000)
    , (offsetDateTime 1970 1 1 11 59 59 0 (offsetOfSeconds' (-1)), 86400000)
    ]
  where
    test arg@(datetime, milli) =
      it (show arg) $ getEpochMilli datetime `shouldBe` milli
