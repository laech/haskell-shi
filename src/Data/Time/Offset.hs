module Data.Time.Offset
  ( Offset
  , HasOffset(..)
  , getTotalOffsetSeconds
  , offsetOfSeconds
  , utcOffset
  ) where

import Control.Monad.Fail
import Data.Int
import Data.Maybe
import Prelude hiding (fail)

-- | A time offset from UTC.
newtype Offset =
  Offset Int32
  deriving (Eq, Ord)

class HasOffset a where
  getOffset :: a -> Offset
  setOffset :: Offset -> a -> a

instance HasOffset Offset where
  getOffset = id
  setOffset = const

getTotalOffsetSeconds :: Offset -> Int
getTotalOffsetSeconds (Offset seconds) = fromIntegral seconds

-- | Create an instance with offset in the given seconds. Errors if
-- value is not within -18:00 and +18:00.
offsetOfSeconds :: MonadFail m => Int -> m Offset
offsetOfSeconds seconds =
  if offset < minBound || offset > maxBound
    then fail errmsg
    else pure offset
  where
    offset = Offset $ fromIntegral seconds
    errmsg = show seconds ++ " is not between " ++ minstr ++ " and " ++ maxstr
    minstr = show $ getTotalOffsetSeconds minBound
    maxstr = show $ getTotalOffsetSeconds maxBound

utcOffset :: Offset
utcOffset = fromJust $ offsetOfSeconds 0

instance Bounded Offset where
  maxBound = Offset $ 18 * 60 * 60
  minBound = Offset $ 18 * 60 * 60 * (-1)

instance Show Offset where
  show offset = show' $ getTotalOffsetSeconds offset

show' :: Int -> String
show' 0 = "Z"
show' offsetSeconds =
  sign ++ pad hours ++ ":" ++ pad minutes ++ showIfNonZero seconds
  where
    (hours, (minutes, seconds)) =
      (`divMod` 60) <$> abs offsetSeconds `divMod` 3600
    sign =
      if offsetSeconds < 0
        then "-"
        else "+"
    pad i =
      if i >= 10
        then show i
        else "0" ++ show i
    showIfNonZero i =
      if i == 0
        then ""
        else ":" ++ pad i
