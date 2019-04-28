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
  } deriving (Eq, Ord, Show)

instance Bounded Offset where
  maxBound = Offset $ 18 * 60 * 60
  minBound = Offset $ -(getTotalOffsetSeconds maxBound)

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
