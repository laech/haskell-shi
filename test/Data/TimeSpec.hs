module Data.TimeSpec where

import           Data.Time
import           Test.Hspec

spec :: Spec
spec = instantSpec

instantSpec :: Spec
instantSpec = describe "Instant" $ do

  it "equals another when values equal"
     ((Instant 1 2 `compare` Instant 1 2) `shouldBe` EQ)

  it "less than another when value is less" $ do
    (Instant 1 2 `compare` Instant 1 3) `shouldBe` LT
    (Instant 1 2 `compare` Instant 2 2) `shouldBe` LT

  it "greater than another when value is greater" $ do
    (Instant 1 3 `compare` Instant 1 2) `shouldBe` GT
    (Instant 2 2 `compare` Instant 1 2) `shouldBe` GT

  it "`toEpochMillis` returns 0 for epoch time"
     (toEpochMillis (Instant 0 0) `shouldBe` 0)

  it "`toEpochMillis` returns correct value for time in future" $ do
    toEpochMillis (Instant 1 2) `shouldBe` 1000
    toEpochMillis (Instant 11 200000000) `shouldBe` 11200

  it "`toEpochMillis` returns correct value for time in past" $ do
    toEpochMillis (Instant (-1) 2) `shouldBe` 999
    toEpochMillis (Instant 1 200000000) `shouldBe` 800
