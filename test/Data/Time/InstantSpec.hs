module Data.Time.InstantSpec where

import Data.List (sort)
import Data.Maybe
import Data.Time.Instant
import Test.Hspec

spec :: Spec
spec =
  describe "Instant" $ do
    describe "compare" instantCompareSpec
    describe "instantOfEpocMilli" instantOfEpochMilliSpec
    describe "getEpochSecond" getEpochSecondSpec
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
instantOfEpochMilliSpec =
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

getEpochSecondSpec :: Spec
getEpochSecondSpec =
  mapM_
    test
    [ (instantOfEpochMilli 0, 0)
    , (instantOfEpochMilli 1000, 1)
    , (instantOfEpochMilli 1001, 1)
    , (instantOfEpochMilli 2000, 2)
    , (instantOfEpochMilli (-1000), -1)
    , (instantOfEpochSecond 1 0, 1)
    , (instantOfEpochSecond 1 2, 1)
    , (instantOfEpochSecond (-1) 2, -1)
    ]
  where
    test arg@(instant, sec) =
      it (show arg) $ getEpochSecond instant `shouldBe` sec

getEpochMilliSpec :: Spec
getEpochMilliSpec =
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
