{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Time.Format
  ( parseInstant
  ) where

import Control.Monad.Fail
import Data.Text (Text)
import Data.Time.Instant
import Data.Time.LocalDate
import Data.Time.LocalDateTime
import Data.Time.LocalTime
import Data.Time.Month
import Data.Time.Year
import Prelude hiding (fail)
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Text

yearParser :: Parser Integer
yearParser = do
  neg <- (True <$ char '-') <|> pure False
  year <- read <$> many1 digit
  pure $
    if neg
      then -year
      else year

monthParser :: Parser Month
monthParser = do
  value <- read <$> count 2 digit
  case monthOf value of
    Just month -> pure month
    _ -> fail $ "Invalid month: " ++ show value

dayParser :: Integer -> Month -> Parser Int
dayParser year month = do
  day <- read <$> count 2 digit
  if day <= getDaysInMonth (isLeapYear year) month
    then pure day
    else fail $ "Invalid day of month: " ++ show day

hourParser :: Parser Int
hourParser = do
  hour <- read <$> count 2 digit
  if hour < 24
    then pure hour
    else fail $ "Invalid hour: " ++ show hour

minuteOrSecondParser :: Parser Int
minuteOrSecondParser = do
  value <- read <$> count 2 digit
  if value < 60
    then pure value
    else fail $ "Invalid value: " ++ show value

nanoParser :: Parser Int
nanoParser = do
  chars <- many1 digit
  let len = length chars
  if len <= 9
    then pure $ read chars * (10 ^ (9 - len))
    else fail "Too many fractional digits."

localDateParser :: Parser LocalDate
localDateParser = do
  year <- yearParser <* char '-'
  month <- monthParser <* char '-'
  day <- dayParser year month
  localDateOf' year month day

localTimeParser :: Parser LocalTime
localTimeParser = do
  hour <- hourParser <* char ':'
  minute <- minuteOrSecondParser <* char ':'
  second <- minuteOrSecondParser
  fraction <- try (char '.') <|> pure '0'
  nano <-
    if fraction == '.'
      then nanoParser
      else pure 0
  localTimeOf hour minute second nano

instantParser :: Parser Instant
instantParser = do
  date <- localDateParser <* char 'T'
  time <- localTimeParser <* char 'Z'
  let datetime = LocalDateTime date time
  pure $
    instantOfEpochSecond (getEpochSecond datetime) (getNanoOfSecond datetime)

parseInstant :: MonadFail m => Text -> m Instant
parseInstant input =
  case parse instantParser "" input of
    Left e -> fail . show . map messageString . errorMessages $ e
    Right v -> pure v
