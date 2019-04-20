{-# LANGUAGE OverloadedStrings #-}

module Data.Time.FormatSpec where

import Data.Time.Format
import Data.Time.Instant
import Test.Hspec

spec :: Spec
spec = describe "parseInstant" parseInstantSpec

parseInstantSpec :: Spec
parseInstantSpec =
  mapM_
    test
    [ ("1970-01-01T00:00:00Z", Just epoch)
    , ("1970-01-01T00:00:00.0Z", Just epoch)
    , ("1970-01-01T00:00:00.00Z", Just epoch)
    , ("1970-01-01T00:00:00.000Z", Just epoch)
    , ("1970-01-01T00:00:00.00000Z", Just epoch)
    , ("1970-01-01T00:00:00.000000Z", Just epoch)
    , ("1970-01-01T00:00:00.0000000Z", Just epoch)
    , ("1970-01-01T00:00:00.00000000Z", Just epoch)
    , ("1970-01-01T00:00:00.000000000Z", Just epoch)
    , ("1970-01-01T00:00:00.001000000Z", Just (instantOfEpochMilli 1))
    , ("1969-12-31T23:59:59.999Z", Just (instantOfEpochMilli (-1)))
    ]
  where
    test (input, result) =
      it (show input) $ parseInstant input `shouldBe` result
