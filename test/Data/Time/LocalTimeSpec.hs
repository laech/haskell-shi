module Data.Time.LocalTimeSpec where

import Data.List (sort)
import Data.Maybe
import Data.Time.LocalTime
import Test.Hspec

spec :: Spec
spec =
  describe "LocalTime" $ do
    describe "compare" localTimeCompareSpec
    describe "localTimeOf" localTimeOfSpec
    describe "getSecondOfDay" getSecondOfDaySpec

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
