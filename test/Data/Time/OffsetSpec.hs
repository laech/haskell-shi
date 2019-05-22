module Data.Time.OffsetSpec where

import Data.Maybe
import Data.Time.Offset
import Test.Hspec

spec :: Spec
spec =
  describe "Offset" $ do
    describe "show" showSpec
    describe "compare" compareSpec
    describe "bounded" boundedSpec
    describe "offsetOfSeconds" offsetOfSecondsSpec
    describe "utcOffset" utcOffsetSpec

offsetOfSeconds' :: Int -> Offset
offsetOfSeconds' = fromJust . offsetOfSeconds

showSpec :: Spec
showSpec =
  mapM_
    test
    [ ("Z", offsetOfSeconds' 0)
    , ("+00:00:01", offsetOfSeconds' 1)
    , ("+00:01", offsetOfSeconds' 60)
    , ("+00:01:01", offsetOfSeconds' 61)
    , ("+01:00", offsetOfSeconds' 3600)
    , ("+18:00", offsetOfSeconds' 64800)
    , ("-00:00:01", offsetOfSeconds' (-1))
    , ("-00:01", offsetOfSeconds' (-60))
    , ("+10:30", offsetOfSeconds' 37800)
    , ("-10:30", offsetOfSeconds' (-37800))
    ]
  where
    test (str, offset) = it str $ show offset `shouldBe` str

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
