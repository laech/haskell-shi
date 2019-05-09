module Data.Time.OffsetDateTimeSpec where

import Data.Maybe
import Data.Time.Month
import Data.Time.OffsetDateTime
import Test.Hspec

spec :: Spec
spec =
  describe "OffsetDateTime" $ do
    describe "show" showSpec
    describe "compare" compareSpec
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
    describe "getEpochMilli" getEpochMilliSpec
    describe "setYear" setYearSpec
    describe "setMonth" setMonthSpec
    describe "setLocalDate" setLocalDateSpec
    describe "setLocalTime" setLocalTimeSpec
    describe "setLocalDateTime" setLocalDateTimeSpec
    describe "addNanos" addNanosSpec
    describe "addSeconds" addSecondsSpec
    describe "addMinutes" addMinutesSpec
    describe "addHours" addHoursSpec
    describe "addDays" addDaysSpec
    describe "addMonths" addMonthsSpec
    describe "addYears" addYearsSpec

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
    test (str, dt) = it (show str) $ show dt `shouldBe` str

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ ( EQ
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset)
    , ( LT
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 1 0 (offsetOfSeconds' 1))
    , ( LT
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 0 1 utcOffset)
    , ( LT
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' (-1)))
    , ( GT
      , offsetDateTime 1970 1 1 0 0 1 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset)
    , ( GT
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 1))
    , ( GT
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset
      , offsetDateTime 1969 12 31 23 59 59 0 (offsetOfSeconds' (-1)))
    , ( LT
      , offsetDateTime 1970 1 1 0 0 0 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 1 0 (offsetOfSeconds' 1))
    , ( GT
      , offsetDateTime 1970 1 1 0 0 1 0 utcOffset
      , offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' (-1)))
    ]
  where
    test arg@(result, a, b) = it (show arg) $ a `compare` b `shouldBe` result

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
    , (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 1), -1000)
    , (offsetDateTime 1970 1 1 0 0 0 1 (offsetOfSeconds' (-1)), 1000)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' 43200), 0)
    , (offsetDateTime 1970 1 1 12 0 0 2000000 (offsetOfSeconds' 43199), 1002)
    , (offsetDateTime 1970 1 1 12 0 0 0 (offsetOfSeconds' 43201), -1000)
    , (offsetDateTime 1970 1 2 0 0 0 0 utcOffset, 86400000)
    , (offsetDateTime 1970 1 1 23 59 59 0 utcOffset, 86399000)
    , (offsetDateTime 1970 1 1 23 59 59 0 (offsetOfSeconds' (-1)), 86400000)
    ]
  where
    test arg@(datetime, milli) =
      it (show arg) $ getEpochMilli datetime `shouldBe` milli

setYearSpec :: Spec
setYearSpec =
  it "should set the correct year" $
  setYear 1234 (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 8)) `shouldBe`
  offsetDateTime 1234 1 1 0 0 0 0 (offsetOfSeconds' 8)

setMonthSpec :: Spec
setMonthSpec =
  it "should set the correct month" $
  setMonth March (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 8)) `shouldBe`
  offsetDateTime 1970 3 1 0 0 0 0 (offsetOfSeconds' 8)

setLocalDateSpec :: Spec
setLocalDateSpec =
  it "should set the correct date" $
  setLocalDate
    (fromJust $ fromDate 1999 3 9)
    (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 8)) `shouldBe`
  offsetDateTime 1999 3 9 0 0 0 0 (offsetOfSeconds' 8)

setLocalTimeSpec :: Spec
setLocalTimeSpec =
  it "should set the correct time" $
  setLocalTime
    (fromJust $ fromTime 1 2 3 4)
    (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 8)) `shouldBe`
  offsetDateTime 1970 1 1 1 2 3 4 (offsetOfSeconds' 8)

setLocalDateTimeSpec :: Spec
setLocalDateTimeSpec =
  it "should set the correct date time" $
  setLocalDateTime
    (fromJust $ fromDateTime 1999 3 7 1 2 3 4)
    (offsetDateTime 1970 1 1 0 0 0 0 (offsetOfSeconds' 8)) `shouldBe`
  offsetDateTime 1999 3 7 1 2 3 4 (offsetOfSeconds' 8)

addNanosSpec :: Spec
addNanosSpec =
  it "should add nanoseconds to date time" $
  addNanos 999999999 (offsetDateTime 1970 1 1 2 3 4 5 (offsetOfSeconds' 9)) `shouldBe`
  offsetDateTime 1970 1 1 2 3 5 4 (offsetOfSeconds' 9)

addSecondsSpec :: Spec
addSecondsSpec =
  it "should add seconds to date time" $
  addSeconds 123 (offsetDateTime 1970 1 1 2 3 4 5 (offsetOfSeconds' 9)) `shouldBe`
  offsetDateTime 1970 1 1 2 5 7 5 (offsetOfSeconds' 9)

addMinutesSpec :: Spec
addMinutesSpec =
  it "should add minutes to date time" $
  addMinutes 123 (offsetDateTime 1970 1 1 2 3 4 5 (offsetOfSeconds' 9)) `shouldBe`
  offsetDateTime 1970 1 1 4 6 4 5 (offsetOfSeconds' 9)

addHoursSpec :: Spec
addHoursSpec =
  it "should add hours to date time" $
  addHours 123 (offsetDateTime 1970 1 1 2 3 4 5 (offsetOfSeconds' 9)) `shouldBe`
  offsetDateTime 1970 1 6 5 3 4 5 (offsetOfSeconds' 9)

addDaysSpec :: Spec
addDaysSpec =
  it "should add days to date time" $
  addDays 123 (offsetDateTime 1970 1 1 2 3 4 5 (offsetOfSeconds' 9)) `shouldBe`
  offsetDateTime 1970 5 4 2 3 4 5 (offsetOfSeconds' 9)

addMonthsSpec :: Spec
addMonthsSpec =
  it "should add months to date time" $
  addMonths 123 (offsetDateTime 1970 1 1 2 3 4 5 (offsetOfSeconds' 9)) `shouldBe`
  offsetDateTime 1980 4 1 2 3 4 5 (offsetOfSeconds' 9)

addYearsSpec :: Spec
addYearsSpec =
  it "should add years to date time" $
  addYears 123 (offsetDateTime 1970 3 6 2 3 4 5 (offsetOfSeconds' 9)) `shouldBe`
  offsetDateTime 2093 3 6 2 3 4 5 (offsetOfSeconds' 9)
