module Data.Time.Offset
  ( Offset(getTotalOffsetSeconds)
  , offsetOfSeconds
  , utcOffset
  ) where

import Control.Monad.Fail
import Data.Maybe
import Prelude hiding (fail)

-- | A time offset from UTC.
newtype Offset = Offset
  { getTotalOffsetSeconds :: Int
  } deriving (Eq, Ord)

-- | Create an instance with offset in the given seconds. Errors if
-- value is not within -18:00 and +18:00.
offsetOfSeconds :: MonadFail m => Int -> m Offset
offsetOfSeconds seconds =
  if offset < minBound || offset > maxBound
    then fail errmsg
    else pure offset
  where
    offset = Offset seconds
    errmsg = show seconds ++ " is not between " ++ minstr ++ " and " ++ maxstr
    minstr = show $ getTotalOffsetSeconds minBound
    maxstr = show $ getTotalOffsetSeconds maxBound

utcOffset :: Offset
utcOffset = fromJust $ offsetOfSeconds 0

instance Bounded Offset where
  maxBound = Offset $ 18 * 60 * 60
  minBound = Offset $ -(getTotalOffsetSeconds maxBound)

instance Show Offset where
  show offset = show' $ getTotalOffsetSeconds offset

show' :: Int -> String
show' 0 = "Z"
show' offsetSeconds =
  sign ++ pad hours ++ ":" ++ pad minutes ++ showIfNonZero seconds
  where
    (hours, (minutes, seconds)) =
      flip divMod 60 <$> divMod (abs offsetSeconds) 3600
    sign =
      if offsetSeconds < 0
        then "-"
        else "+"
    pad i =
      if i > 10
        then show i
        else "0" ++ show i
    showIfNonZero i =
      if i == 0
        then ""
        else ":" ++ pad i
