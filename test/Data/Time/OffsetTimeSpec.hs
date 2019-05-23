{-# LANGUAGE FlexibleContexts #-}

module Data.Time.OffsetTimeSpec where

import Data.Maybe
import Data.Time.OffsetTime
import Test.Hspec

spec :: Spec
spec =
  describe "OffsetTime" $ do
    describe "show" showSpec
    describe "compare" compareSpec
    describe "bounded" boundedSpec
    describe "fromTime" fromTimeSpec
    describe "fromNanoOfDay" fromNanoOfDaySpec
    describe "fromSecondOfDay" fromSecondOfDaySpec
    describe "getHour" getHourSpec
    describe "getMinute" getMinuteSpec
    describe "getSecond" getSecondSpec
    describe "getNanoOfSecond" getNanoOfSecondSpec
    describe "getLocalTime" getLocalTimeSpec
    describe "getSecondOfDay" getSecondOfDaySpec
    describe "getNanoOfDay" getNanoOfDaySpec
    describe "addHours" addHoursSpec
    describe "addMinutes" addMinutesSpec
    describe "addSeconds" addSecondsSpec
    describe "addNanos" addNanosSpec

offsetTime :: Int -> Int -> Int -> Int -> Offset -> OffsetTime
offsetTime hour minute second nano offset =
  fromJust $ fromTime hour minute second nano offset

offsetOfSeconds' :: Int -> Offset
offsetOfSeconds' = fromJust . offsetOfSeconds

showSpec :: Spec
showSpec =
  mapM_
    test
    [ ("00:00Z", offsetTime 0 0 0 0 utcOffset)
    , ("00:00:00.1Z", offsetTime 0 0 0 100000000 utcOffset)
    , ("00:00:01+00:02", offsetTime 0 0 1 0 (offsetOfSeconds' 120))
    , ("18:01:02-10:30", offsetTime 18 1 2 0 (offsetOfSeconds' (-37800)))
    ]
  where
    test (str, time) = it str $ show time `shouldBe` str

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, minBound, minBound)
    , (EQ, maxBound, maxBound)
    , (LT, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 0 1 utcOffset)
    , (GT, offsetTime 0 0 0 1 utcOffset, offsetTime 0 0 0 0 utcOffset)
    , ( LT
      , offsetTime 0 0 0 0 utcOffset
      , offsetTime 0 0 1 0 (offsetOfSeconds' 1))
    , ( GT
      , offsetTime 0 0 1 0 utcOffset
      , offsetTime 0 0 0 0 (offsetOfSeconds' (-1)))
    , ( GT
      , offsetTime 0 0 1 0 (offsetOfSeconds' 1)
      , offsetTime 0 0 0 0 utcOffset)
    , ( LT
      , offsetTime 0 0 0 0 (offsetOfSeconds' (-1))
      , offsetTime 0 0 1 0 utcOffset)
    ]
  where
    test arg@(result, a, b) = it (show arg) $ compare a b `shouldBe` result

boundedSpec :: Spec
boundedSpec = do
  it "minBound" $ minBound `shouldBe` offsetTime 0 0 0 0 minBound
  it "maxBound" $ maxBound `shouldBe` offsetTime 23 59 59 999999999 maxBound

fromTimeSpec :: Spec
fromTimeSpec =
  mapM_
    test
    [ (-1, 0, 0, 0, utcOffset, Nothing)
    , (0, -1, 0, 0, utcOffset, Nothing)
    , (0, 0, -1, 0, utcOffset, Nothing)
    , (0, 0, 0, -1, utcOffset, Nothing)
    , (24, 0, 0, 0, utcOffset, Nothing)
    , (0, 60, 0, 0, utcOffset, Nothing)
    , (0, 0, 60, 0, utcOffset, Nothing)
    , (0, 0, 0, 1000000000, utcOffset, Nothing)
    , (0, 0, 0, 0, utcOffset, Just (offsetTime 0 0 0 0 utcOffset))
    , (23, 0, 0, 0, utcOffset, Just (offsetTime 23 0 0 0 utcOffset))
    , (0, 59, 0, 0, utcOffset, Just (offsetTime 0 59 0 0 utcOffset))
    , (0, 0, 59, 0, utcOffset, Just (offsetTime 0 0 59 0 utcOffset))
    ]
  where
    test arg@(hour, minute, second, nano, offset, expected) =
      it (show arg) $
      fromTime hour minute second nano offset `shouldBe` expected

fromNanoOfDaySpec :: Spec
fromNanoOfDaySpec =
  mapM_
    test
    [ (-1, utcOffset, Nothing)
    , (86400000000000, utcOffset, Nothing)
    , ( 86399999999999
      , utcOffset
      , Just $ offsetTime 23 59 59 999999999 utcOffset)
    , (0, offsetOfSeconds' 11, Just $ offsetTime 0 0 0 0 (offsetOfSeconds' 11))
    , (1, utcOffset, Just $ offsetTime 0 0 0 1 utcOffset)
    ]
  where
    test (nano, offset, time) =
      it (show nano) $ fromNanoOfDay nano offset `shouldBe` time

fromSecondOfDaySpec :: Spec
fromSecondOfDaySpec =
  mapM_
    test
    [ (-1, utcOffset, Nothing)
    , (86400, utcOffset, Nothing)
    , (86399, utcOffset, Just $ offsetTime 23 59 59 0 utcOffset)
    , (0, offsetOfSeconds' 11, Just $ offsetTime 0 0 0 0 (offsetOfSeconds' 11))
    , (1, offsetOfSeconds' 12, Just $ offsetTime 0 0 1 0 (offsetOfSeconds' 12))
    , (59, utcOffset, Just $ offsetTime 0 0 59 0 utcOffset)
    ]
  where
    test (second, offset, time) =
      it (show second) $ fromSecondOfDay second offset `shouldBe` time

getHourSpec :: Spec
getHourSpec =
  mapM_
    test
    [ (offsetTime 0 2 3 4 utcOffset, 0)
    , (offsetTime 1 2 3 4 utcOffset, 1)
    , (offsetTime 23 2 3 4 utcOffset, 23)
    ]
  where
    test arg@(time, hour) = it (show arg) $ getHour time `shouldBe` hour

getMinuteSpec :: Spec
getMinuteSpec =
  mapM_
    test
    [ (offsetTime 10 0 2 3 utcOffset, 0)
    , (offsetTime 10 1 2 3 utcOffset, 1)
    , (offsetTime 10 59 2 3 utcOffset, 59)
    ]
  where
    test arg@(time, minute) = it (show arg) $ getMinute time `shouldBe` minute

getSecondSpec :: Spec
getSecondSpec =
  mapM_
    test
    [ (offsetTime 10 10 0 2 utcOffset, 0)
    , (offsetTime 10 10 1 2 utcOffset, 1)
    , (offsetTime 10 10 59 2 utcOffset, 59)
    ]
  where
    test arg@(time, second) = it (show arg) $ getSecond time `shouldBe` second

getNanoOfSecondSpec :: Spec
getNanoOfSecondSpec =
  mapM_
    test
    [ (offsetTime 10 10 10 0 utcOffset, 0)
    , (offsetTime 10 10 10 1 utcOffset, 1)
    , (offsetTime 10 10 10 999999999 utcOffset, 999999999)
    ]
  where
    test arg@(time, nano) = it (show arg) $ getNanoOfSecond time `shouldBe` nano

getLocalTimeSpec :: Spec
getLocalTimeSpec =
  mapM_
    test
    [ (offsetTime 10 10 10 0 utcOffset, fromJust $ fromTime 10 10 10 0)
    , (offsetTime 10 10 10 1 utcOffset, fromJust $ fromTime 10 10 10 1)
    , ( offsetTime 10 10 10 999999999 utcOffset
      , fromJust $ fromTime 10 10 10 999999999)
    ]
  where
    test arg@(oTime, lTime) =
      it (show arg) $ getLocalTime oTime `shouldBe` lTime

getSecondOfDaySpec :: Spec
getSecondOfDaySpec =
  mapM_
    test
    [ (0, 0, 0, 0, utcOffset, 0)
    , (1, 0, 0, 0, utcOffset, 3600)
    , (0, 1, 0, 0, utcOffset, 60)
    , (0, 0, 1, 0, utcOffset, 1)
    , (0, 0, 0, 1, utcOffset, 0)
    , (1, 1, 1, 0, utcOffset, 3661)
    ]
  where
    test arg@(hour, minute, second, nano, offset, expected) =
      it (show arg) $
      getSecondOfDay (offsetTime hour minute second nano offset) `shouldBe`
      expected

getNanoOfDaySpec :: Spec
getNanoOfDaySpec =
  mapM_
    test
    [ (offsetTime 0 0 0 0 utcOffset, 0)
    , (offsetTime 0 0 0 1 utcOffset, 1)
    , (offsetTime 0 0 1 0 utcOffset, 1000000000)
    , (offsetTime 0 1 0 0 utcOffset, 60000000000)
    , (offsetTime 1 0 0 0 utcOffset, 3600000000000)
    , (offsetTime 23 59 59 999999999 utcOffset, 86399999999999)
    ]
  where
    test (time, nano) = it (show time) $ getNanoOfDay time `shouldBe` nano

addHoursSpec :: Spec
addHoursSpec =
  mapM_
    test
    [ (0, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 0 0 utcOffset)
    , (1, offsetTime 0 0 0 0 utcOffset, offsetTime 1 0 0 0 utcOffset)
    , (-1, offsetTime 0 0 0 0 utcOffset, offsetTime 23 0 0 0 utcOffset)
    , (23, offsetTime 0 0 0 0 utcOffset, offsetTime 23 0 0 0 utcOffset)
    , (-999999999, offsetTime 0 0 0 0 utcOffset, offsetTime 9 0 0 0 utcOffset)
    , (-1, offsetTime 0 8 9 10 utcOffset, offsetTime 23 8 9 10 utcOffset)
    , (23, offsetTime 0 11 12 13 utcOffset, offsetTime 23 11 12 13 utcOffset)
    ]
  where
    test arg@(hours, oldTime, newTime) =
      it (show arg) $ addHours hours oldTime `shouldBe` newTime

addMinutesSpec :: Spec
addMinutesSpec =
  mapM_
    test
    [ (0, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 0 0 utcOffset)
    , (61, offsetTime 0 0 0 0 utcOffset, offsetTime 1 1 0 0 utcOffset)
    , (-123, offsetTime 0 0 0 0 utcOffset, offsetTime 21 57 0 0 utcOffset)
    , (999999999, offsetTime 0 0 0 0 utcOffset, offsetTime 10 39 0 0 utcOffset)
    , (0, offsetTime 1 2 3 4 utcOffset, offsetTime 1 2 3 4 utcOffset)
    , (1, offsetTime 0 5 6 7 utcOffset, offsetTime 0 6 6 7 utcOffset)
    ]
  where
    test arg@(minutes, oldTime, newTime) =
      it (show arg) $ addMinutes minutes oldTime `shouldBe` newTime

addSecondsSpec :: Spec
addSecondsSpec =
  mapM_
    test
    [ (0, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 0 0 utcOffset)
    , (1, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 1 0 utcOffset)
    , (59, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 59 0 utcOffset)
    , (60, offsetTime 0 0 0 0 utcOffset, offsetTime 0 1 0 0 utcOffset)
    , (123, offsetTime 0 0 0 0 utcOffset, offsetTime 0 2 3 0 utcOffset)
    , (999999999, offsetTime 0 0 0 0 utcOffset, offsetTime 01 46 39 0 utcOffset)
    , (-1, offsetTime 0 8 9 10 utcOffset, offsetTime 0 8 8 10 utcOffset)
    ]
  where
    test arg@(seconds, oldTime, newTime) =
      it (show arg) $ addSeconds seconds oldTime `shouldBe` newTime

addNanosSpec :: Spec
addNanosSpec =
  mapM_
    test
    [ (0, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 0 0 utcOffset)
    , (1, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 0 1 utcOffset)
    , (1000000000, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 1 0 utcOffset)
    , (1000000001, offsetTime 0 0 0 0 utcOffset, offsetTime 0 0 1 1 utcOffset)
    , (0, offsetTime 1 2 3 4 utcOffset, offsetTime 1 2 3 4 utcOffset)
    , (1, offsetTime 0 5 6 7 utcOffset, offsetTime 0 5 6 8 utcOffset)
    , (-1, offsetTime 0 8 9 10 utcOffset, offsetTime 0 8 9 9 utcOffset)
    ]
  where
    test arg@(nanos, oldTime, newTime) =
      it (show arg) $ addNanos nanos oldTime `shouldBe` newTime
