module Data.Time.LocalDateTimeSpec where

import Data.Maybe
import Data.Time.LocalDate
import Data.Time.LocalDateTime
import Data.Time.LocalTime
import Test.Hspec

spec :: Spec
spec =
  describe "LocalDateTime" $ do
    describe "compare" compareSpec
    describe "getYear" getYearSpec
    describe "getMonth" getMonthSpec
    describe "getDayOfMonth" getDayOfMonthSpec
    describe "getDayOfYear" getDayOfYearSpec
    describe "getHour" getHourSpec
    describe "getMinute" getMinuteSpec
    describe "getSecond" getSecondSpec
    describe "getNanoOfSecond" getNanoOfSecondSpec
    describe "getSecondOfDay" getSecondOfDaySpec
    describe "getEpochDay" getEpochDaySpec
    describe "getEpochSecond" getEpochSecondSpec
    describe "getEpochMilli" getEpochMilliSpec
    describe "addDays" addDaysSpec

localDateTime ::
     Integer -> Int -> Int -> Int -> Int -> Int -> Int -> LocalDateTime
localDateTime year month day hour minute second nano =
  LocalDateTime
    (fromJust (localDateOf year month day))
    (fromJust (localTimeOf hour minute second nano))

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, localDateTime 1 1 1 1 1 1 1, localDateTime 1 1 1 1 1 1 1)
    , (LT, localDateTime 1 1 1 1 1 1 1, localDateTime 1 1 1 1 1 1 2)
    , (LT, localDateTime 1 1 1 1 1 1 1, localDateTime 1 1 1 1 1 2 1)
    , (LT, localDateTime 1 1 1 1 1 1 1, localDateTime 1 1 1 1 2 1 1)
    , (LT, localDateTime 1 1 1 1 1 1 1, localDateTime 1 1 1 2 1 1 1)
    , (LT, localDateTime 1 1 1 1 1 1 1, localDateTime 1 1 2 1 1 1 1)
    , (LT, localDateTime 1 1 1 1 1 1 1, localDateTime 1 2 1 1 1 1 1)
    , (LT, localDateTime 1 1 1 1 1 1 1, localDateTime 2 1 1 1 1 1 1)
    , (GT, localDateTime 1 1 1 1 1 1 2, localDateTime 1 1 1 1 1 1 1)
    , (GT, localDateTime 1 1 1 1 1 2 1, localDateTime 1 1 1 1 1 1 1)
    , (GT, localDateTime 1 1 1 1 2 1 1, localDateTime 1 1 1 1 1 1 1)
    , (GT, localDateTime 1 1 1 2 1 1 1, localDateTime 1 1 1 1 1 1 1)
    , (GT, localDateTime 1 1 2 1 1 1 1, localDateTime 1 1 1 1 1 1 1)
    , (GT, localDateTime 1 2 1 1 1 1 1, localDateTime 1 1 1 1 1 1 1)
    , (GT, localDateTime 2 1 1 1 1 1 1, localDateTime 1 1 1 1 1 1 1)
    ]
  where
    test arg@(expect, a, b) = it (show arg) $ a `compare` b `shouldBe` expect

getYearSpec :: Spec
getYearSpec =
  mapM_
    test
    [ (localDateTime 0 2 3 4 5 6 7, 0)
    , (localDateTime 1 2 3 4 5 6 7, 1)
    , (localDateTime (-1) 2 3 4 5 6 7, -1)
    ]
  where
    test arg@(datetime, year) = it (show arg) $ getYear datetime `shouldBe` year

getMonthSpec :: Spec
getMonthSpec =
  mapM_
    test
    [ (localDateTime 10 1 2 3 4 5 6, 1)
    , (localDateTime 10 7 2 3 4 5 6, 7)
    , (localDateTime 10 9 2 3 4 5 6, 9)
    ]
  where
    test arg@(datetime, month) =
      it (show arg) $ getMonth datetime `shouldBe` month

getDayOfMonthSpec :: Spec
getDayOfMonthSpec =
  mapM_
    test
    [(localDateTime 10 10 11 2 3 4 5, 11), (localDateTime 10 10 9 2 3 4 5, 9)]
  where
    test arg@(datetime, day) =
      it (show arg) $ getDayOfMonth datetime `shouldBe` day

getDayOfYearSpec :: Spec
getDayOfYearSpec =
  mapM_
    test
    [ (localDateTime 1970 1 1 1 2 3 4, 1)
    , (localDateTime 1970 1 2 0 0 0 0, 2)
    , (localDateTime 1970 2 1 9 8 7 6, 32)
    , (localDateTime 1970 12 31 0 0 0 0, 365)
    , (localDateTime 2000 2 28 0 0 0 0, 59)
    , (localDateTime 2000 2 29 2 2 2 2, 60)
    , (localDateTime 2000 3 1 0 0 0 0, 61)
    , (localDateTime 2000 12 31 0 0 0 0, 366)
    ]
  where
    test arg@(datetime, dayOfYear) =
      it (show arg) $ getDayOfYear datetime `shouldBe` dayOfYear

getHourSpec :: Spec
getHourSpec =
  mapM_
    test
    [ (localDateTime 10 10 10 0 2 3 4, 0)
    , (localDateTime 10 10 10 1 2 3 4, 1)
    , (localDateTime 10 10 10 23 2 3 4, 23)
    ]
  where
    test arg@(datetime, hour) = it (show arg) $ getHour datetime `shouldBe` hour

getMinuteSpec :: Spec
getMinuteSpec =
  mapM_
    test
    [ (localDateTime 10 10 10 10 0 2 3, 0)
    , (localDateTime 10 10 10 10 1 2 3, 1)
    , (localDateTime 10 10 10 10 59 2 3, 59)
    ]
  where
    test arg@(datetime, minute) =
      it (show arg) $ getMinute datetime `shouldBe` minute

getSecondSpec :: Spec
getSecondSpec =
  mapM_
    test
    [ (localDateTime 10 10 10 10 10 0 2, 0)
    , (localDateTime 10 10 10 10 10 1 2, 1)
    , (localDateTime 10 10 10 10 10 59 2, 59)
    ]
  where
    test arg@(datetime, second) =
      it (show arg) $ getSecond datetime `shouldBe` second

getNanoOfSecondSpec :: Spec
getNanoOfSecondSpec =
  mapM_
    test
    [ (localDateTime 10 10 10 10 10 10 0, 0)
    , (localDateTime 10 10 10 10 10 10 1, 1)
    , (localDateTime 10 10 10 10 10 10 999999999, 999999999)
    ]
  where
    test arg@(datetime, nano) =
      it (show arg) $ getNanoOfSecond datetime `shouldBe` nano

getSecondOfDaySpec :: Spec
getSecondOfDaySpec =
  mapM_
    test
    [ (localDateTime 9 9 9 0 0 0 9, 0)
    , (localDateTime 9 9 9 0 0 1 9, 1)
    , (localDateTime 9 9 9 0 0 59 9, 59)
    , (localDateTime 9 9 9 0 1 0 9, 60)
    , (localDateTime 9 9 9 0 1 2 9, 62)
    , (localDateTime 9 9 9 0 2 0 9, 120)
    , (localDateTime 9 9 9 0 2 1 9, 121)
    , (localDateTime 9 9 9 1 0 0 9, 3600)
    , (localDateTime 9 9 9 1 1 0 9, 3660)
    , (localDateTime 9 9 9 1 1 1 9, 3661)
    ]
  where
    test arg@(datetime, second) =
      it (show arg) $ getSecondOfDay datetime `shouldBe` second

getEpochDaySpec :: Spec
getEpochDaySpec =
  mapM_
    test
    [ (localDateTime 1970 1 1 0 0 0 0, 0)
    , (localDateTime 1970 1 1 9 9 9 9, 0)
    , (localDateTime 1970 1 2 0 0 0 0, 1)
    , (localDateTime 1970 1 2 9 9 9 9, 1)
    , (localDateTime 1969 12 31 0 0 0 0, -1)
    , (localDateTime 1969 12 31 9 9 9 9, -1)
    ]
  where
    test arg@(datetime, day) =
      it (show arg) $ getEpochDay datetime `shouldBe` day

getEpochSecondSpec :: Spec
getEpochSecondSpec =
  mapM_
    test
    [ (localDateTime 1970 1 1 0 0 0 0, 0)
    , (localDateTime 1970 1 1 0 0 0 9, 0)
    , (localDateTime 1970 1 1 0 0 1 0, 1)
    , (localDateTime 1970 1 1 0 0 1 9, 1)
    , (localDateTime 1969 12 31 23 59 59 0, -1)
    , (localDateTime 1969 12 31 23 59 59 9, -1)
    ]
  where
    test arg@(datetime, second) =
      it (show arg) $ getEpochSecond datetime `shouldBe` second

getEpochMilliSpec :: Spec
getEpochMilliSpec =
  mapM_
    test
    [ (localDateTime 1970 1 1 0 0 0 0, 0)
    , (localDateTime 1970 1 1 0 0 0 999999, 0)
    , (localDateTime 1970 1 1 0 0 0 1000000, 1)
    , (localDateTime 1970 1 1 0 0 0 1999999, 1)
    , (localDateTime 1969 12 31 23 59 59 999999999, -1)
    , (localDateTime 1969 12 31 23 59 59 999000000, -1)
    ]
  where
    test arg@(datetime, milli) =
      it (show arg) $ getEpochMilli datetime `shouldBe` milli

addDaysSpec :: Spec
addDaysSpec =
  mapM_
    test
    [ (localDateTime 1970 1 1 0 0 0 0, 0, localDateTime 1970 1 1 0 0 0 0)
    , (localDateTime 1970 1 1 9 8 7 6, 1, localDateTime 1970 1 2 9 8 7 6)
    , (localDateTime 1970 1 1 0 0 0 0, 2, localDateTime 1970 1 3 0 0 0 0)
    , (localDateTime 1970 1 1 0 0 0 0, -1, localDateTime 1969 12 31 0 0 0 0)
    , (localDateTime 1970 1 1 0 0 0 0, -2, localDateTime 1969 12 30 0 0 0 0)
    , (localDateTime 2000 2 28 5 6 7 8, 1, localDateTime 2000 2 29 5 6 7 8)
    , (localDateTime 2000 2 28 0 0 0 0, 2, localDateTime 2000 3 1 0 0 0 0)
    , (localDateTime 2000 1 1 0 0 0 1, 365, localDateTime 2000 12 31 0 0 0 1)
    , (localDateTime 2000 1 1 0 0 0 2, 366, localDateTime 2001 1 1 0 0 0 2)
    ]
  where
    test arg@(fromDateTime, days, toDateTime) =
      it (show arg) $ addDays days fromDateTime `shouldBe` toDateTime
