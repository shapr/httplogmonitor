{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Monad                  (unless)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Attoparsec.Text
import qualified Data.Set                       as S
import           Data.Text                      hiding (length, replicate,
                                                 zipWith)
import           Chronos.Types
import qualified Chronos
import           Hedgehog
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Gen.QuickCheck        as Gen
import qualified Hedgehog.Range                 as Range
import           LogParser
import           System.Exit                    (exitFailure)
import           Test.QuickCheck.Instances.Time ()
import           Net.Types (IPv4)
import qualified Net.IPv4 as IPv4

-- generate and save a random log message value
-- write the generated value out to a string
-- parse the string
-- check to see if the parsed value matches the original generated value
-- https://teh.id.au/posts/2017/06/07/round-trip-property/
prop_roundtrip :: Property
prop_roundtrip =
    withTests 1000 . property $
              do
                lm <- forAll genLogMessage
                tripping lm (pack . showLM) (parseOnly pLogMessage)

now :: IO Datetime
now = Chronos.timeToDatetime <$> Chronos.now

{-
-- get the current time
-- replicate the current time 10 min * 60 sec * 10 perSecond + 1
-- zip the list of times with addUTCTime [-1,-2..] producing a list of times each one second older than previous
-- generate a random log message
-- zip the list of times with an infinite list of the duplicated random log message, replacing timestamp values
checkAlert :: Property
checkAlert = property $ do
  currentTime <- liftIO now
  thresholdValue <- forAll $ Gen.int (Range.linear 1 99)
  let eventCountToGenerate = 10 * 60 * thresholdValue -- see the test fail if you add - 1 after thresholdValue
  let tenPerSecond = replicate eventCountToGenerate currentTime
  let eachOneSecondOlder = zipWith addUTCTime [-1,-2..] tenPerSecond
  randomLogMessage <- Gen.sample genLogMessage -- create one sample LogMessage value
  let tenPSLogMessages = zipWith setTimeStamp (cycle [randomLogMessage] )  eachOneSecondOlder
  let tenPSset = S.fromList tenPSLogMessages
  let alertResult = detectAlert tenPSset thresholdValue currentTime
  -- liftIO $ print alertResult
  assert $ length (alertResult) == 1

setTimeStamp lm ts = lm { timestamp = ts }
-}

-- generate a random log message
genLogMessage :: Gen LogMessage
genLogMessage =
    LM <$> genIPv4
       <*> Gen.text (Range.linear 1 12) Gen.alpha
       <*> genDatetime
       <*> Gen.text (Range.linear 1 4) Gen.alphaNum
       <*> (Gen.list (Range.linear 1 12) $ Gen.text (Range.constant 1 12) Gen.alpha)
       <*> Gen.int (Range.linear 100 500)
       <*> Gen.int (Range.linear 100 1544)

genIPv4 :: Gen IPv4
genIPv4 = do
  let rng = IPv4.toList $ IPv4.fromBounds (IPv4.fromOctets 192 168 16 0) (IPv4.fromOctets 192 168 19 255)
  let len = length rng
  ix <- Gen.int (Range.linear 0 (len - 1))
  pure $ rng !! ix
 
genDate :: Gen Date
genDate = Date <$> genYear <*> genMonth <*> genDayOfMonth

genYear :: Gen Year
genYear = Year <$> Gen.int (Range.linear 1500 2400)

genMonth :: Gen Month
genMonth = Month <$> Gen.int (Range.linear 0 11)

genDayOfMonth :: Gen DayOfMonth
genDayOfMonth = DayOfMonth <$> Gen.int (Range.linear 1 27)

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
  hour <- Gen.int (Range.linear 0 23)
  minute <- Gen.int (Range.linear 0 59)
  seconds <- Gen.int64 (Range.linear 0 59)
  pure (TimeOfDay hour minute (seconds * 1000000000))
  
genDatetime :: Gen Datetime
genDatetime = Datetime <$> genDate <*> genTimeOfDay

tests :: IO Bool
tests = do
  checkSequential $ Group "httplogmonitor" [
                       ("prop_roundtrip" , prop_roundtrip)
--                       , ("prop_alert", checkAlert)
                      ]
main :: IO ()
main = do
  results <- sequence [ tests ]
  unless (and results) $ exitFailure
