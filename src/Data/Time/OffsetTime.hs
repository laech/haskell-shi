{-# LANGUAGE FlexibleInstances #-}

module Data.Time.OffsetTime
  ( OffsetTime
  , module Data.Time.Base
  , module Data.Time.LocalTime
  , module Data.Time.Offset
  ) where

import Control.Monad.Fail
import Data.Ord
import Data.Time.Base
import Data.Time.LocalTime
import Data.Time.Offset

-- | A time with an offset.
data OffsetTime =
  OffsetTime {-# UNPACK #-}!Offset
             !LocalTime
  deriving (Eq)

instance MonadFail m => FromTime (Offset -> m OffsetTime) where
  fromTime hour minute second nano offset =
    OffsetTime offset <$> fromTime hour minute second nano

instance MonadFail m => FromNanoOfDay (Offset -> m OffsetTime) where
  fromNanoOfDay nano offset = (`fromLocalTime` offset) <$> fromNanoOfDay nano

instance MonadFail m => FromSecondOfDay (Offset -> m OffsetTime) where
  fromSecondOfDay second offset =
    (`fromLocalTime` offset) <$> fromSecondOfDay second

instance FromLocalTime (Offset -> OffsetTime) where
  fromLocalTime = flip OffsetTime

instance Show OffsetTime where
  show (OffsetTime offset time) = show time ++ show offset

instance Ord OffsetTime where
  compare a b
    | offsetEq || epochNanoEq = comparing getLocalTime a b
    | otherwise = epochNanoOrd
    where
      offsetEq = comparing getOffset a b == EQ
      epochNanoEq = epochNanoOrd == EQ
      epochNanoOrd = comparing getEpochNano a b
      getEpochNano (OffsetTime offset time) =
        getNanoOfDay time - fromIntegral (getTotalOffsetSeconds offset)

instance Bounded OffsetTime where
  minBound = OffsetTime minBound minBound
  maxBound = OffsetTime maxBound maxBound

instance HasOffset OffsetTime where
  getOffset (OffsetTime offset _) = offset
  setOffset offset' x@(OffsetTime offset time)
    | offset' == offset = x
    | otherwise = OffsetTime offset' time

instance HasLocalTime OffsetTime where
  getLocalTime (OffsetTime _ time) = time
  setLocalTime time' x@(OffsetTime offset time)
    | time' == time = x
    | otherwise = OffsetTime offset time'

instance HasHour OffsetTime where
  getHour = getHour . getLocalTime
  addHours n = modifyLocalTime (addHours n)

instance HasMinute OffsetTime where
  getMinute = getMinute . getLocalTime
  addMinutes n = modifyLocalTime (addMinutes n)

instance HasSecond OffsetTime where
  getSecond = getSecond . getLocalTime
  addSeconds n = modifyLocalTime (addSeconds n)

instance HasNanoOfSecond OffsetTime where
  getNanoOfSecond = getNanoOfSecond . getLocalTime
  addNanos n = modifyLocalTime (addNanos n)

instance HasSecondOfDay OffsetTime where
  getSecondOfDay = getSecondOfDay . getLocalTime

instance HasNanoOfDay OffsetTime where
  getNanoOfDay = getNanoOfDay . getLocalTime
