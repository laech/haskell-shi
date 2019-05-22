{-# LANGUAGE FlexibleInstances #-}

module Data.Time.LocalTime
  ( LocalTime
  , HasLocalTime(..)
  , addTime
  , module Data.Time.Base
  ) where

import Control.Monad.Fail
import Data.List
import Data.Maybe
import Data.Time.Base
import Data.Word
import Prelude hiding (fail)

-- | Time without time zone.
data LocalTime =
  LocalTime {-# UNPACK #-}!Word8
            {-# UNPACK #-}!Word8
            {-# UNPACK #-}!Word8
            {-# UNPACK #-}!Word32
  deriving (Eq, Ord)

class ( HasHour a
      , HasMinute a
      , HasSecond a
      , HasNanoOfSecond a
      , HasNanoOfDay a
      , HasSecondOfDay a
      ) =>
      HasLocalTime a
  where
  getLocalTime :: a -> LocalTime
  setLocalTime :: LocalTime -> a -> a
  modifyLocalTime :: (LocalTime -> LocalTime) -> a -> a
  modifyLocalTime f a = setLocalTime (f $ getLocalTime a) a

instance HasLocalTime LocalTime where
  getLocalTime = id
  setLocalTime = const

instance MonadFail m => FromTime (m LocalTime) where
  fromTime = fromTime'

fromTime' :: MonadFail m => Int -> Int -> Int -> Int -> m LocalTime
fromTime' hour minute second nano =
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

instance MonadFail m => FromSecondOfDay (m LocalTime) where
  fromSecondOfDay = fromSecondOfDay'

fromSecondOfDay' :: MonadFail m => Int -> m LocalTime
fromSecondOfDay' secondOfDay =
  if secondOfDay < 0 || secondOfDay >= 86400
    then fail $ "Invalid second of day: " ++ show secondOfDay
    else fromTime' hour minute second 0
  where
    (hour, (minute, second)) = (`divMod` 60) <$> secondOfDay `divMod` 3600

instance MonadFail m => FromNanoOfDay (m LocalTime) where
  fromNanoOfDay = fromNanoOfDay'

fromNanoOfDay' :: MonadFail m => Integer -> m LocalTime
fromNanoOfDay' nanoOfDay =
  if nanoOfDay < 0 || nanoOfDay >= 86400000000000
    then fail $ "Invalid nano of day: " ++ show nanoOfDay
    else fromTime'
           (fromIntegral hour)
           (fromIntegral minute)
           (fromIntegral second)
           (fromIntegral nano)
  where
    (hour, (minute, (second, nano))) =
      (fmap . fmap)
        (`divMod` nsPerSecond)
        ((`divMod` nsPerMinute) <$> divMod nanoOfDay nsPerHour)

nsPerSecond :: Integer
nsPerSecond = 1000000000

nsPerMinute :: Integer
nsPerMinute = nsPerSecond * 60

nsPerHour :: Integer
nsPerHour = nsPerMinute * 60

nsPerDay :: Integer
nsPerDay = nsPerHour * 24

-- | Adds the given amount of hours, minutes, seconds, and nanoseconds
-- to a time, returns a new time and the number of days overfollowed
-- from the addition.
addTime ::
     Int -- ^ Hours
  -> Int -- ^ Minutes
  -> Int -- ^ Seconds
  -> Int -- ^ Nanoseconds
  -> LocalTime
  -> (Int, LocalTime)
addTime 0 0 0 0 time = (0, time)
addTime hours minutes seconds nanos time = (fromIntegral days, time')
  where
    (days, time') =
      fromJust . fromNanoOfDay <$>
      divMod
        ((fromIntegral (getHour time) + fromIntegral hours) * nsPerHour +
         (fromIntegral (getMinute time) + fromIntegral minutes) * nsPerMinute +
         (fromIntegral (getSecond time) + fromIntegral seconds) * nsPerSecond +
         (fromIntegral (getNanoOfSecond time) + fromIntegral nanos))
        nsPerDay

instance HasHour LocalTime where
  getHour (LocalTime h _ _ _) = fromIntegral h
  addHours 0 = id
  addHours n = snd . addTime n 0 0 0

instance HasMinute LocalTime where
  getMinute (LocalTime _ m _ _) = fromIntegral m
  addMinutes 0 = id
  addMinutes n = snd . addTime 0 n 0 0

instance HasSecond LocalTime where
  getSecond (LocalTime _ _ s _) = fromIntegral s
  addSeconds 0 = id
  addSeconds n = snd . addTime 0 0 n 0

instance HasNanoOfSecond LocalTime where
  getNanoOfSecond (LocalTime _ _ _ n) = fromIntegral n
  addNanos 0 = id
  addNanos n = snd . addTime 0 0 0 n

instance HasSecondOfDay LocalTime where
  getSecondOfDay time =
    getHour time * 60 * 60 + getMinute time * 60 + getSecond time

instance HasNanoOfDay LocalTime where
  getNanoOfDay time =
    fromIntegral (getHour time) * nsPerHour +
    fromIntegral (getMinute time) * nsPerMinute +
    fromIntegral (getSecond time) * nsPerSecond +
    fromIntegral (getNanoOfSecond time)

instance Show LocalTime where
  show = show'

show' :: LocalTime -> String
show' (LocalTime hour minute second nano) = hourS ++ ":" ++ minuteS ++ suffix
  where
    suffix
      | nano > 0 = ":" ++ secondS ++ "." ++ nanoS
      | second > 0 = ":" ++ secondS
      | otherwise = ""
    hourS = pad 2 . show $ hour
    minuteS = pad 2 . show $ minute
    secondS = pad 2 . show $ second
    nanoS = dropWhileEnd (== '0') $ pad 9 . show $ nano
    pad width str =
      let len = length str
       in if len >= width
            then str
            else replicate (width - len) '0' ++ str

instance Bounded LocalTime where
  minBound = fromJust $ fromNanoOfDay 0
  maxBound = fromJust $ fromNanoOfDay 86399999999999
