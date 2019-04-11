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
    describe "getHour" getHourSpec
    describe "getMinute" getMinuteSpec
    describe "getSecond" getSecondSpec
    describe "getNanoOfSecond" getNanoOfSecondSpec
    describe "getSecondOfDay" getSecondOfDaySpec

localDateTimeValid ::
     Integer -> Int -> Int -> Int -> Int -> Int -> Int -> LocalDateTime
localDateTimeValid year month day hour minute second nano =
  LocalDateTime
    (fromJust (localDateOf year month day))
    (fromJust (localTimeOf hour minute second nano))

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 1 1 1 1 1 1 1)
    , (LT, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 1 1 1 1 1 1 2)
    , (LT, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 1 1 1 1 1 2 1)
    , (LT, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 1 1 1 1 2 1 1)
    , (LT, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 1 1 1 2 1 1 1)
    , (LT, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 1 1 2 1 1 1 1)
    , (LT, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 1 2 1 1 1 1 1)
    , (LT, localDateTimeValid 1 1 1 1 1 1 1, localDateTimeValid 2 1 1 1 1 1 1)
    , (GT, localDateTimeValid 1 1 1 1 1 1 2, localDateTimeValid 1 1 1 1 1 1 1)
    , (GT, localDateTimeValid 1 1 1 1 1 2 1, localDateTimeValid 1 1 1 1 1 1 1)
    , (GT, localDateTimeValid 1 1 1 1 2 1 1, localDateTimeValid 1 1 1 1 1 1 1)
    , (GT, localDateTimeValid 1 1 1 2 1 1 1, localDateTimeValid 1 1 1 1 1 1 1)
    , (GT, localDateTimeValid 1 1 2 1 1 1 1, localDateTimeValid 1 1 1 1 1 1 1)
    , (GT, localDateTimeValid 1 2 1 1 1 1 1, localDateTimeValid 1 1 1 1 1 1 1)
    , (GT, localDateTimeValid 2 1 1 1 1 1 1, localDateTimeValid 1 1 1 1 1 1 1)
    ]
  where
    test arg@(expect, a, b) = it (show arg) $ a `compare` b `shouldBe` expect

getYearSpec :: Spec
getYearSpec =
  mapM_
    test
    [ (localDateTimeValid 0 2 3 4 5 6 7, 0)
    , (localDateTimeValid 1 2 3 4 5 6 7, 1)
    , (localDateTimeValid (-1) 2 3 4 5 6 7, -1)
    ]
  where
    test arg@(datetime, year) = it (show arg) $ getYear datetime `shouldBe` year

getMonthSpec :: Spec
getMonthSpec =
  mapM_
    test
    [ (localDateTimeValid 10 1 2 3 4 5 6, 1)
    , (localDateTimeValid 10 7 2 3 4 5 6, 7)
    , (localDateTimeValid 10 9 2 3 4 5 6, 9)
    ]
  where
    test arg@(datetime, month) =
      it (show arg) $ getMonth datetime `shouldBe` month

getDayOfMonthSpec :: Spec
getDayOfMonthSpec =
  mapM_
    test
    [ (localDateTimeValid 10 10 11 2 3 4 5, 11)
    , (localDateTimeValid 10 10 9 2 3 4 5, 9)
    ]
  where
    test arg@(datetime, day) =
      it (show arg) $ getDayOfMonth datetime `shouldBe` day

getHourSpec :: Spec
getHourSpec =
  mapM_
    test
    [ (localDateTimeValid 10 10 10 0 2 3 4, 0)
    , (localDateTimeValid 10 10 10 1 2 3 4, 1)
    , (localDateTimeValid 10 10 10 23 2 3 4, 23)
    ]
  where
    test arg@(datetime, hour) = it (show arg) $ getHour datetime `shouldBe` hour

getMinuteSpec :: Spec
getMinuteSpec =
  mapM_
    test
    [ (localDateTimeValid 10 10 10 10 0 2 3, 0)
    , (localDateTimeValid 10 10 10 10 1 2 3, 1)
    , (localDateTimeValid 10 10 10 10 59 2 3, 59)
    ]
  where
    test arg@(datetime, minute) =
      it (show arg) $ getMinute datetime `shouldBe` minute

getSecondSpec :: Spec
getSecondSpec =
  mapM_
    test
    [ (localDateTimeValid 10 10 10 10 10 0 2, 0)
    , (localDateTimeValid 10 10 10 10 10 1 2, 1)
    , (localDateTimeValid 10 10 10 10 10 59 2, 59)
    ]
  where
    test arg@(datetime, second) =
      it (show arg) $ getSecond datetime `shouldBe` second

getNanoOfSecondSpec :: Spec
getNanoOfSecondSpec =
  mapM_
    test
    [ (localDateTimeValid 10 10 10 10 10 10 0, 0)
    , (localDateTimeValid 10 10 10 10 10 10 1, 1)
    , (localDateTimeValid 10 10 10 10 10 10 999999999, 999999999)
    ]
  where
    test arg@(datetime, nano) =
      it (show arg) $ getNanoOfSecond datetime `shouldBe` nano

getSecondOfDaySpec :: Spec
getSecondOfDaySpec =
  mapM_
    test
    [ (localDateTimeValid 9 9 9 0 0 0 9, 0)
    , (localDateTimeValid 9 9 9 0 0 1 9, 1)
    , (localDateTimeValid 9 9 9 0 0 59 9, 59)
    , (localDateTimeValid 9 9 9 0 1 0 9, 60)
    , (localDateTimeValid 9 9 9 0 1 2 9, 62)
    , (localDateTimeValid 9 9 9 0 2 0 9, 120)
    , (localDateTimeValid 9 9 9 0 2 1 9, 121)
    , (localDateTimeValid 9 9 9 1 0 0 9, 3600)
    , (localDateTimeValid 9 9 9 1 1 0 9, 3660)
    , (localDateTimeValid 9 9 9 1 1 1 9, 3661)
    ]
  where
    test arg@(datetime, second) =
      it (show arg) $ getSecondOfDay datetime `shouldBe` second