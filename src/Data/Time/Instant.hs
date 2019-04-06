module Data.Time.Instant
  ( Instant
  , epoch
  , instantOfEpochSecond
  , instantOfEpochMilli
  , getEpochMilli
  ) where

import Control.Monad.Fail
import Data.Time.Base
import Data.Word
import Prelude hiding (fail)

data Instant =
  Instant Integer
          Word32
  deriving (Eq, Ord, Show)

instance HasEpochSecond Instant where
  getEpochSecond (Instant s _) = s

instance HasNanoOfSecond Instant where
  getNanoOfSecond (Instant _ n) = fromIntegral n

-- | Converts an instant to the millisecond since 'epoch'.
getEpochMilli :: Instant -> Integer
getEpochMilli (Instant sec ns) = sec * 1000 + fromIntegral (ns `div` 1000000)

-- | 1970-01-01T00:00:00Z
epoch :: Instant
epoch = Instant 0 0

instantOfEpochSecond :: Integer -> Int -> Instant
instantOfEpochSecond second nano = Instant second (fromIntegral nano) -- TODO

-- | Converts a millisecond from 'epoch' to an instant.
instantOfEpochMilli :: Integer -> Instant
instantOfEpochMilli ms = Instant sec ns
  where
    sec = ms `div` 1000
    ns = fromIntegral (ms - sec * 1000) * 1000000
