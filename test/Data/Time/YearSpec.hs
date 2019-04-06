module Data.Time.YearSpec where

import Data.Time.Year
import Test.Hspec

spec :: Spec
spec =
  describe "Year" $ do
    describe "isLeapYear" isLeapYearSpec
    describe "getDaysInYear" getDaysInYearSpec

isLeapYearSpec :: Spec
isLeapYearSpec = do
  it "if it's divisible by 4" $ isLeapYear 4 `shouldBe` True
  it "but not divisible by 100" $ isLeapYear 1700 `shouldBe` False
  it "unless it's divisible by 400" $ isLeapYear 1600 `shouldBe` True
  it "false otherwise" $ isLeapYear 2019 `shouldBe` False

getDaysInYearSpec :: Spec
getDaysInYearSpec = do
  it "returns 366 for a leap year" $ getDaysInYear 1600 `shouldBe` 366
  it "returns 365 for a non leap year" $ getDaysInYear 2011 `shouldBe` 365
