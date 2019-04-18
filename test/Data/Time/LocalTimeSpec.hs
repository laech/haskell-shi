module Data.Time.LocalTimeSpec where

import Data.Maybe
import Data.Time.LocalTime
import Test.Hspec

spec :: Spec
spec =
  describe "LocalTime" $ do
    describe "show" showSpec
    describe "compare" localTimeCompareSpec
    describe "localTimeOf" localTimeOfSpec
    describe "getSecondOfDay" getSecondOfDaySpec

localTimeValid :: Int -> Int -> Int -> Int -> LocalTime
localTimeValid h m s n = fromJust (localTimeOf h m s n)

showSpec :: Spec
showSpec =
  mapM_
    test
    [ (localTimeValid 1 2 3 9, "01:02:03.000000009")
    , (localTimeValid 1 2 3 90, "01:02:03.00000009")
    , (localTimeValid 1 2 3 900, "01:02:03.0000009")
    , (localTimeValid 1 2 3 9000, "01:02:03.000009")
    , (localTimeValid 1 2 3 90000, "01:02:03.00009")
    , (localTimeValid 1 2 3 900000, "01:02:03.0009")
    , (localTimeValid 1 2 3 9000000, "01:02:03.009")
    , (localTimeValid 1 2 3 90000000, "01:02:03.09")
    , (localTimeValid 1 2 3 900000000, "01:02:03.9")
    , (localTimeValid 1 2 0 900000000, "01:02:00.9")
    , (localTimeValid 1 2 3 0, "01:02:03")
    , (localTimeValid 1 2 0 0, "01:02")
    , (localTimeValid 1 0 0 0, "01:00")
    , (localTimeValid 11 22 33 100000000, "11:22:33.1")
    , (localTimeValid 11 22 33 120000000, "11:22:33.12")
    , (localTimeValid 11 22 33 123000000, "11:22:33.123")
    , (localTimeValid 11 22 33 123400000, "11:22:33.1234")
    , (localTimeValid 11 22 33 123450000, "11:22:33.12345")
    , (localTimeValid 11 22 33 123456000, "11:22:33.123456")
    , (localTimeValid 11 22 33 123456700, "11:22:33.1234567")
    , (localTimeValid 11 22 33 123456780, "11:22:33.12345678")
    , (localTimeValid 11 22 33 123456789, "11:22:33.123456789")
    ]
  where
    test (time, str) = it str $ show time `shouldBe` str

localTimeCompareSpec :: Spec
localTimeCompareSpec =
  mapM_
    test
    [ (EQ, localTimeValid 1 2 3 4, localTimeValid 1 2 3 4)
    , (LT, localTimeValid 1 2 3 4, localTimeValid 2 2 3 4)
    , (LT, localTimeValid 1 2 3 4, localTimeValid 1 3 3 4)
    , (LT, localTimeValid 1 2 3 4, localTimeValid 1 2 4 4)
    , (LT, localTimeValid 1 2 3 4, localTimeValid 1 2 3 5)
    , (GT, localTimeValid 2 2 3 4, localTimeValid 1 2 3 4)
    , (GT, localTimeValid 1 3 3 4, localTimeValid 1 2 3 4)
    , (GT, localTimeValid 1 2 4 4, localTimeValid 1 2 3 4)
    , (GT, localTimeValid 1 2 3 5, localTimeValid 1 2 3 4)
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
    , (0, 0, 0, 0, Just (localTimeValid 0 0 0 0))
    , (23, 0, 0, 0, Just (localTimeValid 23 0 0 0))
    , (0, 59, 0, 0, Just (localTimeValid 0 59 0 0))
    , (0, 0, 59, 0, Just (localTimeValid 0 0 59 0))
    , (0, 0, 0, 999999999, Just (localTimeValid 0 0 0 999999999))
    , (1, 2, 3, 4, Just (localTimeValid 1 2 3 4))
    ]
  where
    test arg@(h, m, s, n, expected) =
      it (show arg) $ localTimeOf h m s n `shouldBe` expected

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
      it (show arg) $
      getSecondOfDay (localTimeValid h m s n) `shouldBe` expected
