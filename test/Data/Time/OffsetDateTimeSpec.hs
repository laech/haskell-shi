module Data.Time.OffsetDateTimeSpec where

import Data.Maybe
import Data.Time.OffsetDateTime
import Test.Hspec

spec :: Spec
spec = describe "OffsetDateTime" $ do
  describe "show" showSpec

offsetDateTime ::
     Integer
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Offset
  -> OffsetDateTime
offsetDateTime year month day hour minute second nano offset =
  fromJust $ fromDateTime year month day hour minute second nano offset

offsetOfSeconds' :: Int -> Offset
offsetOfSeconds' seconds = fromJust $ offsetOfSeconds seconds

showSpec :: Spec
showSpec =
  mapM_
    test
    [ ("1970-01-01T00:00Z", offsetDateTime 1970 1 1 0 0 0 0 utcOffset)
    , ( "1970-01-02T03:04:05+01:00"
      , offsetDateTime 1970 1 2 3 4 5 0 (offsetOfSeconds' 3600))
    , ( "1970-01-02T03:04:05-01:00"
      , offsetDateTime 1970 1 2 3 4 5 0 (offsetOfSeconds' (-3600)))
    ]
  where
    test :: (String, OffsetDateTime) -> Spec
    test (str, dt) = it (show str) $ show dt `shouldBe` str
