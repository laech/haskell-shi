module Data.Time.LocalDateSpec where

import Data.Maybe
import Data.Time.LocalDate
import Test.Hspec

spec :: Spec
spec =
  describe "LocalDate" $ do
    describe "show" showSpec
    describe "compare" compareSpec
    describe "addDays" addDaysSpec
    describe "addMonths" addMonthsSpec
    describe "addYears" addYearsSpec
    describe "setYear" setYearSpec
    describe "setMonth" setMonthSpec
    describe "getDayOfYear" getDayOfYearSpec
    describe "getEpochDay" getEpochDaySpec
    describe "localDateOf" localDateOfSpec
    describe "localDateOfEpochDay" localDateOfEpochDaySpec
    describe "localDateOfYearDay" localDateOfYearDaySpec

localDate :: Integer -> Int -> Int -> LocalDate
localDate y m d = fromJust $ localDateOf y m d

showSpec :: Spec
showSpec =
  mapM_
    test
    [ (localDate 1970 1 1, "1970-01-01")
    , (localDate 1970 1 31, "1970-01-31")
    , (localDate 1970 11 1, "1970-11-01")
    , (localDate 0 1 1, "0000-01-01")
    , (localDate 1 1 1, "0001-01-01")
    , (localDate (-1) 1 1, "-0001-01-01")
    , (localDate (-99999) 1 1, "-99999-01-01")
    , (localDate 99999 1 1, "99999-01-01")
    ]
  where
    test (date, str) = it str $ show date `shouldBe` str

compareSpec :: Spec
compareSpec =
  mapM_
    test
    [ (EQ, localDate 1 2 3, localDate 1 2 3)
    , (LT, localDate 1 2 3, localDate 2 2 3)
    , (LT, localDate 1 2 3, localDate 1 3 3)
    , (LT, localDate 1 2 3, localDate 1 2 4)
    , (GT, localDate 2 2 3, localDate 1 2 3)
    , (GT, localDate 1 3 3, localDate 1 2 3)
    , (GT, localDate 1 2 4, localDate 1 2 3)
    ]
  where
    test arg@(expect, a, b) = it (show arg) $ a `compare` b `shouldBe` expect

addDaysSpec :: Spec
addDaysSpec =
  mapM_
    test
    [ (localDate 1970 1 1, 0, localDate 1970 1 1)
    , (localDate 1970 1 1, 1, localDate 1970 1 2)
    , (localDate 1970 1 1, 2, localDate 1970 1 3)
    , (localDate 1970 1 1, -1, localDate 1969 12 31)
    , (localDate 1970 1 1, -2, localDate 1969 12 30)
    , (localDate 2000 2 28, 1, localDate 2000 2 29)
    , (localDate 2000 2 28, 2, localDate 2000 3 1)
    , (localDate 2000 1 1, 365, localDate 2000 12 31)
    , (localDate 2000 1 1, 366, localDate 2001 1 1)
    ]
  where
    test arg@(fromDate, days, toDate) =
      it (show arg) $ addDays days fromDate `shouldBe` toDate

addMonthsSpec :: Spec
addMonthsSpec =
  mapM_
    test
    [ (localDate 1970 1 1, 1, localDate 1970 2 1)
    , (localDate 1970 1 28, 1, localDate 1970 2 28)
    , (localDate 1970 1 31, 1, localDate 1970 2 28)
    , (localDate 1970 1 1, 2, localDate 1970 3 1)
    , (localDate 1970 1 31, 3, localDate 1970 4 30)
    , (localDate 1970 1 1, 12, localDate 1971 1 1)
    , (localDate 1970 1 1, 25, localDate 1972 2 1)
    , (localDate 1970 2 28, 360, localDate 2000 2 28)
    , (localDate 2000 2 29, 12, localDate 2001 2 28)
    , (localDate 1970 1 1, -1, localDate 1969 12 1)
    , (localDate 1970 1 31, -1, localDate 1969 12 31)
    , (localDate 1970 1 3, -1, localDate 1969 12 3)
    , (localDate 1970 1 1, -12, localDate 1969 1 1)
    , (localDate 1970 1 1, -13, localDate 1968 12 1)
    , (localDate 2000 2 29, -12, localDate 1999 2 28)
    , (localDate 0 1 1, 1, localDate 0 2 1)
    , (localDate 0 1 1, -1, localDate (-1) 12 1)
    , (localDate (-1) 1 1, -123, localDate (-12) 10 1)
    , (localDate (-123) 1 1, -123, localDate (-134) 10 1)
    ]
  where
    test arg@(fromDate, months, toDate) =
      it (show arg) $ addMonths months fromDate `shouldBe` toDate

addYearsSpec :: Spec
addYearsSpec =
  mapM_
    test
    [ (localDate 1970 1 1, 1, localDate 1971 1 1)
    , (localDate 2000 2 29, 1, localDate 2001 2 28)
    , (localDate 2001 2 28, -1, localDate 2000 2 28)
    ]
  where
    test arg@(fromDate, years, toDate) =
      it (show arg) $ addYears years fromDate `shouldBe` toDate

setYearSpec :: Spec
setYearSpec =
  mapM_
    test
    [ (localDate 1970 1 1, 0, localDate 0 1 1)
    , (localDate 1970 2 28, 2000, localDate 2000 2 28)
    , (localDate 2000 2 29, 2001, localDate 2001 2 28)
    ]
  where
    test arg@(fromDate, year, toDate) =
      it (show arg) $ setYear year fromDate `shouldBe` toDate

setMonthSpec :: Spec
setMonthSpec =
  mapM_
    test
    [ (localDate 1970 1 1, 1, localDate 1970 1 1)
    , (localDate 1970 1 31, 2, localDate 1970 2 28)
    , (localDate 2000 3 29, 2, localDate 2000 2 29)
    ]
  where
    test arg@(fromDate, month, toDate) =
      it (show arg) $ setMonth (toEnum month) fromDate `shouldBe` toDate

getEpochDaySpec :: Spec
getEpochDaySpec =
  mapM_
    test
    [ (localDate 1970 1 1, 0)
    , (localDate 1970 1 2, 1)
    , (localDate 1970 1 3, 2)
    , (localDate 1970 12 31, 364)
    , (localDate 1971 1 1, 365)
    , (localDate 2004 2 29, 12477)
    , (localDate 2019 3 17, 17972)
    , (localDate 999999999 12 31, 365241780471)
    , (localDate 74556927 9 2, 27230639126)
    , (localDate 941048034 12 12, 343710017376)
    , (localDate 56483748 1 8, 20629545808)
    , (localDate 243946412 7 19, 89098878057)
    , (localDate 78675278 5 11, 28734835828)
    , (localDate 32078367 12 25, 11715663789)
    , (localDate 463697857 4 1, 169361445098)
    , (localDate 443805972 8 24, 162096083436)
    , (localDate 470418915 7 16, 171816261230)
    , (localDate 897678790 5 23, 327869726071)
    , (localDate 162193233 1 15, 59239142391)
    , (localDate 534365546 5 28, 195172288554)
    , (localDate 1969 12 31, -1)
    , (localDate 1969 12 30, -2)
    , (localDate 1969 1 1, -365)
    , (localDate 1960 2 29, -3594)
    , (localDate 0 1 1, -719528)
    , (localDate 0 12 31, -719163)
    , (localDate (-400) 12 31, -865260)
    , (localDate (-1) 12 31, -719529)
    , (localDate (-1) 1 1, -719893)
    , (localDate (-999999999) 1 1, -365243219162)
    , (localDate (-44758093) 11 19, -16348276989)
    , (localDate (-302472524) 10 20, -110476540082)
    , (localDate (-984111839) 12 8, -359440187542)
    , (localDate (-750556618) 8 8, -274135894858)
    , (localDate (-882146331) 9 3, -322198050582)
    , (localDate (-785520990) 2 4, -286906369684)
    , (localDate (-26911170) 6 23, -9829822363)
    , (localDate (-296189792) 6 13, -108181819469)
    ]
  where
    test arg@(date, epochDay) =
      it (show arg) $ getEpochDay date `shouldBe` epochDay

getDayOfYearSpec :: Spec
getDayOfYearSpec =
  mapM_
    test
    [ (localDate 1970 1 1, 1)
    , (localDate 1970 1 2, 2)
    , (localDate 1970 2 1, 32)
    , (localDate 1970 12 31, 365)
    , (localDate 2000 2 28, 59)
    , (localDate 2000 2 29, 60)
    , (localDate 2000 3 1, 61)
    , (localDate 2000 12 31, 366)
    ]
  where
    test arg@(date, dayOfYear) =
      it (show arg) $ getDayOfYear date `shouldBe` dayOfYear

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
    , (1970, 1, 1, Just (localDate 1970 1 1))
    , (2000, 2, 29, Just (localDate 2000 2 29))
    , (-1, 1, 31, Just (localDate (-1) 1 31))
    , (99999, 12, 31, Just (localDate 99999 12 31))
    ]
  where
    test arg@(y, m, d, expected) =
      it (show arg) $ localDateOf y m d `shouldBe` expected

localDateOfEpochDaySpec :: Spec
localDateOfEpochDaySpec =
  mapM_
    test
    [ (localDate 1970 1 1, 0)
    , (localDate 1970 1 2, 1)
    , (localDate 1970 1 3, 2)
    , (localDate 1969 12 31, -1)
    , (localDate 1969 12 30, -2)
    , (localDate 2000 2 28, 11015)
    , (localDate 2000 2 29, 11016)
    , (localDate 2000 3 1, 11017)
    , (localDate (-999999999) 1 1, -365243219162)
    , (localDate 999999999 12 31, 365241780471)
    , (localDate 0 1 1, -719528)
    , (localDate 0 12 31, -719163)
    ]
  where
    test arg@(date, epochDay) =
      it (show arg) $ localDateOfEpochDay epochDay `shouldBe` date

localDateOfYearDaySpec :: Spec
localDateOfYearDaySpec =
  mapM_
    test
    [ (1970, 1, Just $ localDate 1970 1 1)
    , (1970, 31, Just $ localDate 1970 1 31)
    , (1970, 32, Just $ localDate 1970 2 1)
    , (1970, 365, Just $ localDate 1970 12 31)
    , (2000, 31 + 29, Just $ localDate 2000 2 29)
    , (2000, 366, Just $ localDate 2000 12 31)
    , (1970, 366, Nothing)
    , (1970, 0, Nothing)
    , (1970, -1, Nothing)
    ]
  where
    test arg@(year, day, date) =
      it (show arg) $ localDateOfYearDay year day `shouldBe` date
