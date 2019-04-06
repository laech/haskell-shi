module Data.Time.LocalTime
  ( LocalTime
  , getHour
  , getMinute
  , getSecond
  , getSecondOfDay
  , localTimeOf
  , module Data.Time.Base
  ) where

import Control.Monad.Fail
import Data.Time.Base
import Data.Word
import Prelude hiding (fail)

data LocalTime =
  LocalTime Word8
            Word8
            Word8
            Word32
  deriving (Eq, Ord, Show)

getHour :: LocalTime -> Int
getHour (LocalTime h _ _ _) = fromIntegral h

getMinute :: LocalTime -> Int
getMinute (LocalTime _ m _ _) = fromIntegral m

getSecond :: LocalTime -> Int
getSecond (LocalTime _ _ s _) = fromIntegral s

instance HasNanoOfSecond LocalTime where
  getNanoOfSecond (LocalTime _ _ _ n) = fromIntegral n

-- | Get the the time as seconds since the start of the day.
getSecondOfDay :: LocalTime -> Int
getSecondOfDay time =
  getHour time * 60 * 60 + getMinute time * 60 + getSecond time

localTimeOf :: MonadFail m => Int -> Int -> Int -> Int -> m LocalTime
localTimeOf hour minute second nano =
  if hourIsInvalid || minuteIsInvalid || secondIsInvalid || nanoIsInvalid
    then fail $
         "Invalid time: hour=" ++
         show hour ++
         ", minute=" ++
         show minute ++ ", second=" ++ show second ++ ", nano=" ++ show nano
    else pure $
         LocalTime
           (fromIntegral hour)
           (fromIntegral minute)
           (fromIntegral second)
           (fromIntegral nano)
  where
    hourIsInvalid = hour < 0 || hour > 23
    minuteIsInvalid = minute < 0 || minute > 59
    secondIsInvalid = second < 0 || second > 59
    nanoIsInvalid = nano < 0 || nano > 999999999
