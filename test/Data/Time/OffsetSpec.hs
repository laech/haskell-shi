module Data.Time.OffsetSpec where

import Data.Time.Offset
import Test.Hspec

spec :: Spec
spec =
  describe "Offset" $ do
    describe "compare" compareSpec
    describe "offsetOfMinutes" offsetOfMinutesSpec
    describe "offsetOfHoursMinutes" offsetOfHoursMinutesSpec
    describe "utcOffset" utcOffsetSpec

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, offsetOfMinutes 0, offsetOfMinutes 0)
    , (EQ, offsetOfMinutes 1, offsetOfMinutes 1)
    , (EQ, offsetOfMinutes (-1), offsetOfMinutes (-1))
    , (LT, offsetOfMinutes (-1), offsetOfMinutes 1)
    , (GT, offsetOfMinutes 2, offsetOfMinutes 1)
    ]
  where
    test arg@(result, a, b) = it (show arg) $ a `compare` b `shouldBe` result

offsetOfMinutesSpec :: Spec
offsetOfMinutesSpec =
  it "should returns the minutes" $
  getTotalOffsetMinutes (offsetOfMinutes 123) `shouldBe` 123

offsetOfHoursMinutesSpec :: Spec
offsetOfHoursMinutesSpec =
  it "should calculate the minutes" $
  getTotalOffsetMinutes (offsetOfHoursMinutes 1 10) `shouldBe` 70

utcOffsetSpec :: Spec
utcOffsetSpec =
  it "should have offset 0" $ getTotalOffsetMinutes utcOffset `shouldBe` 0
