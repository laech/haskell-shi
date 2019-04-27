module Data.Time.BaseSpec where

import Data.Time.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "modifyYear" modifyYearSpec
  describe "addYears" addYearsSpec

newtype MyYear =
  MyYear Integer
  deriving (Eq, Show)

instance HasYear MyYear where
  getYear (MyYear y) = y
  setYear y _ = MyYear y

modifyYearSpec :: Spec
modifyYearSpec =
  it "should set the right year" $
  modifyYear (+ 1) (MyYear 2) `shouldBe` MyYear 3

addYearsSpec :: Spec
addYearsSpec = do
  it
    "should adds positive number of years"
    (addYears 1 (MyYear 2) `shouldBe` MyYear 3)
  it
    "should adds negative number of years"
    (addYears (-1) (MyYear 2) `shouldBe` MyYear 1)
