module Data.Time.LocalDateSpec where

import Data.Maybe
import Data.Time.LocalDate
import Test.Hspec

spec :: Spec
spec =
  describe "LocalDate" $ do
    describe "compare" compareSpec
    describe "getEpochDay" getEpochDaySpec
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

getEpochDaySpec :: Spec
getEpochDaySpec =
  mapM_
    test
    [ (localDateValid 1970 1 1, 0)
    , (localDateValid 1970 1 2, 1)
    , (localDateValid 1970 1 3, 2)
    , (localDateValid 1970 12 31, 364)
    , (localDateValid 1971 1 1, 365)
    , (localDateValid 2004 2 29, 12477)
    , (localDateValid 2019 3 17, 17972)
    , (localDateValid 999999999 12 31, 365241780471)
    , (localDateValid 74556927 9 2, 27230639126)
    , (localDateValid 941048034 12 12, 343710017376)
    , (localDateValid 56483748 1 8, 20629545808)
    , (localDateValid 243946412 7 19, 89098878057)
    , (localDateValid 78675278 5 11, 28734835828)
    , (localDateValid 32078367 12 25, 11715663789)
    , (localDateValid 463697857 4 1, 169361445098)
    , (localDateValid 443805972 8 24, 162096083436)
    , (localDateValid 470418915 7 16, 171816261230)
    , (localDateValid 897678790 5 23, 327869726071)
    , (localDateValid 162193233 1 15, 59239142391)
    , (localDateValid 534365546 5 28, 195172288554)
    , (localDateValid 1969 12 31, -1)
    , (localDateValid 1969 12 30, -2)
    , (localDateValid 1969 1 1, -365)
    , (localDateValid 1960 2 29, -3594)
    , (localDateValid 0 1 1, -719528)
    , (localDateValid 0 12 31, -719163)
    , (localDateValid (-400) 12 31, -865260)
    , (localDateValid (-1) 12 31, -719529)
    , (localDateValid (-1) 1 1, -719893)
    , (localDateValid (-999999999) 1 1, -365243219162)
    , (localDateValid (-44758093) 11 19, -16348276989)
    , (localDateValid (-302472524) 10 20, -110476540082)
    , (localDateValid (-984111839) 12 8, -359440187542)
    , (localDateValid (-750556618) 8 8, -274135894858)
    , (localDateValid (-882146331) 9 3, -322198050582)
    , (localDateValid (-785520990) 2 4, -286906369684)
    , (localDateValid (-26911170) 6 23, -9829822363)
    , (localDateValid (-296189792) 6 13, -108181819469)
    ]
  where
    test arg@(localDate, epochDay) =
      it (show arg) $ getEpochDay localDate `shouldBe` epochDay

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
