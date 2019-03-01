module Data.TimeSpec where

import           Data.Time
import           Test.Hspec

spec :: Spec
spec = instantSpec

instantSpec = describe "Instant" $ do

  it "equals another when values equal"
     ((Instant 1 2 `compare` Instant 1 2) `shouldBe` EQ)

  it "less than another when value is less" $ do
    (Instant 1 2 `compare` Instant 1 3) `shouldBe` LT
    (Instant 1 2 `compare` Instant 2 2) `shouldBe` LT

  it "greater than another when value is greater" $ do
    (Instant 1 3 `compare` Instant 1 2) `shouldBe` GT
    (Instant 2 2 `compare` Instant 1 2) `shouldBe` GT
