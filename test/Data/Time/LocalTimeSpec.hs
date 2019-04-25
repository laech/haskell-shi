module Data.Time.LocalTimeSpec where

import Data.Maybe
import Data.Time.LocalTime
import Test.Hspec

spec :: Spec
spec =
  describe "LocalTime" $ do
    describe "show" showSpec
    describe "compare" compareSpec
    describe "localTimeOf" localTimeOfSpec
    describe "localTimeOfNanoOfDay" localTimeOfNanoOfDaySpec
    describe "getLocalTime" getLocalTimeSpec
    describe "getSecondOfDay" getSecondOfDaySpec
    describe "getNanoOfDay" getNanoOfDaySpec
    describe "addTime" addTimeSpec
    describe "addHours" addHoursSpec

localTime :: Int -> Int -> Int -> Int -> LocalTime
localTime h m s n = fromJust (localTimeOf h m s n)

showSpec :: Spec
showSpec =
  mapM_
    test
    [ (localTime 1 2 3 9, "01:02:03.000000009")
    , (localTime 1 2 3 90, "01:02:03.00000009")
    , (localTime 1 2 3 900, "01:02:03.0000009")
    , (localTime 1 2 3 9000, "01:02:03.000009")
    , (localTime 1 2 3 90000, "01:02:03.00009")
    , (localTime 1 2 3 900000, "01:02:03.0009")
    , (localTime 1 2 3 9000000, "01:02:03.009")
    , (localTime 1 2 3 90000000, "01:02:03.09")
    , (localTime 1 2 3 900000000, "01:02:03.9")
    , (localTime 1 2 0 900000000, "01:02:00.9")
    , (localTime 1 2 3 0, "01:02:03")
    , (localTime 1 2 0 0, "01:02")
    , (localTime 1 0 0 0, "01:00")
    , (localTime 11 22 33 100000000, "11:22:33.1")
    , (localTime 11 22 33 120000000, "11:22:33.12")
    , (localTime 11 22 33 123000000, "11:22:33.123")
    , (localTime 11 22 33 123400000, "11:22:33.1234")
    , (localTime 11 22 33 123450000, "11:22:33.12345")
    , (localTime 11 22 33 123456000, "11:22:33.123456")
    , (localTime 11 22 33 123456700, "11:22:33.1234567")
    , (localTime 11 22 33 123456780, "11:22:33.12345678")
    , (localTime 11 22 33 123456789, "11:22:33.123456789")
    ]
  where
    test (time, str) = it str $ show time `shouldBe` str

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, localTime 1 2 3 4, localTime 1 2 3 4)
    , (LT, localTime 1 2 3 4, localTime 2 2 3 4)
    , (LT, localTime 1 2 3 4, localTime 1 3 3 4)
    , (LT, localTime 1 2 3 4, localTime 1 2 4 4)
    , (LT, localTime 1 2 3 4, localTime 1 2 3 5)
    , (GT, localTime 2 2 3 4, localTime 1 2 3 4)
    , (GT, localTime 1 3 3 4, localTime 1 2 3 4)
    , (GT, localTime 1 2 4 4, localTime 1 2 3 4)
    , (GT, localTime 1 2 3 5, localTime 1 2 3 4)
    ]
  where
    test arg@(expect, a, b) = it (show arg) $ a `compare` b `shouldBe` expect

localTimeOfSpec :: Spec
localTimeOfSpec =
  mapM_
    test
    [ (-1, 0, 0, 0, Nothing)
    , (0, -1, 0, 0, Nothing)
    , (0, 0, -1, 0, Nothing)
    , (0, 0, 0, -1, Nothing)
    , (24, 0, 0, 0, Nothing)
    , (0, 60, 0, 0, Nothing)
    , (0, 0, 60, 0, Nothing)
    , (0, 0, 0, 1000000000, Nothing)
    , (0, 0, 0, 0, Just (localTime 0 0 0 0))
    , (23, 0, 0, 0, Just (localTime 23 0 0 0))
    , (0, 59, 0, 0, Just (localTime 0 59 0 0))
    , (0, 0, 59, 0, Just (localTime 0 0 59 0))
    , (0, 0, 0, 999999999, Just (localTime 0 0 0 999999999))
    , (1, 2, 3, 4, Just (localTime 1 2 3 4))
    ]
  where
    test arg@(h, m, s, n, expected) =
      it (show arg) $ localTimeOf h m s n `shouldBe` expected

localTimeOfNanoOfDaySpec :: Spec
localTimeOfNanoOfDaySpec =
  mapM_
    test
    [ (-1, Nothing)
    , (86400000000000, Nothing)
    , (86399999999999, Just $ localTime 23 59 59 999999999)
    , (0, Just $ localTime 0 0 0 0)
    , (1, Just $ localTime 0 0 0 1)
    , (999999999, Just $ localTime 0 0 0 999999999)
    , (1000000000, Just $ localTime 0 0 1 0)
    , (60000000000, Just $ localTime 0 1 0 0)
    , (3600000000000, Just $ localTime 1 0 0 0)
    ]
  where
    test (nano, time) =
      it (show nano) $ localTimeOfNanoOfDay nano `shouldBe` time

getLocalTimeSpec :: Spec
getLocalTimeSpec =
  it "should return self" $
  getLocalTime (localTime 1 2 3 4) `shouldBe` localTime 1 2 3 4

getSecondOfDaySpec :: Spec
getSecondOfDaySpec =
  mapM_
    test
    [ (0, 0, 0, 0, 0)
    , (1, 0, 0, 0, 3600)
    , (0, 1, 0, 0, 60)
    , (0, 0, 1, 0, 1)
    , (0, 0, 0, 1, 0)
    , (1, 1, 1, 0, 3661)
    ]
  where
    test arg@(h, m, s, n, expected) =
      it (show arg) $ getSecondOfDay (localTime h m s n) `shouldBe` expected

getNanoOfDaySpec :: Spec
getNanoOfDaySpec =
  mapM_
    test
    [ (localTime 0 0 0 0, 0)
    , (localTime 0 0 0 1, 1)
    , (localTime 0 0 1 0, 1000000000)
    , (localTime 0 1 0 0, 60000000000)
    , (localTime 1 0 0 0, 3600000000000)
    , (localTime 23 59 59 999999999, 86399999999999)
    ]
  where
    test (time, nano) = it (show time) $ getNanoOfDay time `shouldBe` nano

addTimeSpec :: Spec
addTimeSpec =
  mapM_
    test
    [ (0, 0, 0, 0, localTime 0 0 0 0, (0, localTime 0 0 0 0))
    , (0, 0, 0, 1, localTime 0 0 0 0, (0, localTime 0 0 0 1))
    , (0, 0, 0, 1000000000, localTime 0 0 0 0, (0, localTime 0 0 1 0))
    , (0, 0, 1, 0, localTime 0 0 0 0, (0, localTime 0 0 1 0))
    , (0, 0, 1, 0, localTime 0 0 0 0, (0, localTime 0 0 1 0))
    , (24, 0, 0, 0, localTime 0 0 0 0, (1, localTime 0 0 0 0))
    , (-24, 0, 0, 0, localTime 0 0 0 0, (-1, localTime 0 0 0 0))
    , (0, 1440, 0, 0, localTime 0 0 0 0, (1, localTime 0 0 0 0))
    , (0, 0, 86400, 0, localTime 0 0 0 0, (1, localTime 0 0 0 0))
    , (0, 0, 0, 86400000000000, localTime 0 0 0 0, (1, localTime 0 0 0 0))
    ]
  where
    test arg@(hours, minutes, seconds, nanos, time, result) =
      it (show arg) $ addTime hours minutes seconds nanos time `shouldBe` result

addHoursSpec :: Spec
addHoursSpec =
  mapM_
    test
    [ (0, localTime 0 0 0 0, localTime 0 0 0 0)
    , (1, localTime 0 0 0 0, localTime 1 0 0 0)
    , (-1, localTime 0 0 0 0, localTime 23 0 0 0)
    , (23, localTime 0 0 0 0, localTime 23 0 0 0)
    , (24, localTime 0 0 0 0, localTime 0 0 0 0)
    , (25, localTime 0 0 0 0, localTime 1 0 0 0)
    , (123, localTime 0 0 0 0, localTime 3 0 0 0)
    , (-123, localTime 0 0 0 0, localTime 21 0 0 0)
    , (999999999, localTime 0 0 0 0, localTime 15 0 0 0)
    , (-999999999, localTime 0 0 0 0, localTime 9 0 0 0)
    , (0, localTime 1 2 3 4, localTime 1 2 3 4)
    , (1, localTime 0 5 6 7, localTime 1 5 6 7)
    , (-1, localTime 0 8 9 10, localTime 23 8 9 10)
    , (23, localTime 0 11 12 13, localTime 23 11 12 13)
    ]
  where
    test arg@(hours, oldTime, newTime) =
      it (show arg) $ addHours hours oldTime `shouldBe` newTime
