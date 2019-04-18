module Data.Time.LocalTime
  ( LocalTime
  , localTimeOf
  , module Data.Time.Base
  ) where

import Control.Monad.Fail
import Data.List
import Data.Time.Base
import Data.Word
import Prelude hiding (fail)

data LocalTime =
  LocalTime Word8
            Word8
            Word8
            Word32
  deriving (Eq, Ord)

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

instance HasHour LocalTime where
  getHour (LocalTime h _ _ _) = fromIntegral h

instance HasMinute LocalTime where
  getMinute (LocalTime _ m _ _) = fromIntegral m

instance HasSecond LocalTime where
  getSecond (LocalTime _ _ s _) = fromIntegral s

instance HasNanoOfSecond LocalTime where
  getNanoOfSecond (LocalTime _ _ _ n) = fromIntegral n

instance HasSecondOfDay LocalTime where
  getSecondOfDay time =
    getHour time * 60 * 60 + getMinute time * 60 + getSecond time

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
