module Data.Time.OffsetSpec where

import Data.Maybe
import Data.Time.Offset
import Test.Hspec

spec :: Spec
spec =
  describe "Offset" $ do
    describe "compare" compareSpec
    describe "bounded" boundedSpec
    describe "offsetOfSeconds" offsetOfSecondsSpec
    describe "utcOffset" utcOffsetSpec

offsetOfSeconds' :: Int -> Offset
offsetOfSeconds' = fromJust . offsetOfSeconds

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, offsetOfSeconds' 0, offsetOfSeconds' 0)
    , (EQ, offsetOfSeconds' 1, offsetOfSeconds' 1)
    , (EQ, offsetOfSeconds' (-1), offsetOfSeconds' (-1))
    , (LT, offsetOfSeconds' (-1), offsetOfSeconds' 1)
    , (GT, offsetOfSeconds' 2, offsetOfSeconds' 1)
    ]
  where
    test arg@(result, a, b) = it (show arg) $ a `compare` b `shouldBe` result

boundedSpec :: Spec
boundedSpec = do
  it "minBound is -18:00" $
    minBound `shouldBe` fromJust (offsetOfSeconds (-18 * 60 * 60))
  it "maxBound is +18:00" $
    maxBound `shouldBe` fromJust (offsetOfSeconds (18 * 60 * 60))

offsetOfSecondsSpec :: Spec
offsetOfSecondsSpec =
  it "should returns the minutes" $
  getTotalOffsetSeconds (offsetOfSeconds' 123) `shouldBe` 123

utcOffsetSpec :: Spec
utcOffsetSpec =
  it "should have offset 0" $ getTotalOffsetSeconds utcOffset `shouldBe` 0
