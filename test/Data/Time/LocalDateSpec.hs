module Data.Time.LocalDateSpec where

import Data.Maybe
import Data.Time.LocalDate
import Test.Hspec

spec :: Spec
spec =
  describe "LocalDate" $ do
    describe "compare" compareSpec
    describe "localDateOf" localDateOfSpec

localDateValid :: Integer -> Int -> Int -> LocalDate
localDateValid y m d = fromJust $ localDateOf y m d

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, localDateValid 1 2 3, localDateValid 1 2 3)
    , (LT, localDateValid 1 2 3, localDateValid 2 2 3)
    , (LT, localDateValid 1 2 3, localDateValid 1 3 3)
    , (LT, localDateValid 1 2 3, localDateValid 1 2 4)
    , (GT, localDateValid 2 2 3, localDateValid 1 2 3)
    , (GT, localDateValid 1 3 3, localDateValid 1 2 3)
    , (GT, localDateValid 1 2 4, localDateValid 1 2 3)
    ]
  where
    test arg@(expect, a, b) = it (show arg) $ a `compare` b `shouldBe` expect

localDateOfSpec :: Spec
localDateOfSpec =
  mapM_
    test
    [ (1970, 2, 29, Nothing)
    , (1970, 0, 1, Nothing)
    , (1970, 1, 0, Nothing)
    , (2000, 1, 90, Nothing)
    , (2011, -1, 1, Nothing)
    , (2018, 1, -3, Nothing)
    , (1970, 1, 1, Just (localDateValid 1970 1 1))
    , (2000, 2, 29, Just (localDateValid 2000 2 29))
    , (-1, 1, 31, Just (localDateValid (-1) 1 31))
    , (99999, 12, 31, Just (localDateValid 99999 12 31))
    ]
  where
    test arg@(y, m, d, expected) =
      it (show arg) $ localDateOf y m d `shouldBe` expected
