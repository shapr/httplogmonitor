{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Monad                  (unless)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Attoparsec.Text
import qualified Data.Set                       as S
import           Data.Text                      hiding (length, replicate,
                                                 zipWith)
import           Data.Time
import           Hedgehog
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Gen.QuickCheck        as Gen
import qualified Hedgehog.Range                 as Range
import           LogParser
import           System.Exit                    (exitFailure)
import           Test.QuickCheck.Instances.Time ()

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

-- get the current time
-- replicate the current time 10 min * 60 sec * 10 perSecond + 1
-- zip the list of times with addUTCTime [-1,-2..] producing a list of times each one second older than previous
-- generate a random log message
-- zip the list of times with an infinite list of the duplicated random log message, replacing timestamp values
checkAlert :: Property
checkAlert = property $ do
  currentTime <- liftIO getCurrentTime
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

-- generate a random log message
genLogMessage :: Gen LogMessage
genLogMessage =
    LM <$> Gen.text (Range.linear 1 11) Gen.alpha
       <*> Gen.text (Range.linear 1 12) Gen.alpha
       <*> (roundUTC <$> Gen.arbitrary :: Gen UTCTime)
       <*> Gen.text (Range.linear 1 4) Gen.alphaNum
       <*> (Gen.list (Range.linear 1 12) $ Gen.text (Range.constant 1 12) Gen.alpha)
       <*> Gen.int (Range.linear 100 500)
       <*> Gen.int (Range.linear 100 1544)


-- randomly generated time values could produce times that were not an integer number of seconds, due to leap seconds
-- the w3c string representation of time has max resolution of integer seconds
-- thus breaking the round trip property and forcing me to round times to the nearest second
roundUTC' :: UTCTime -> UTCTime
roundUTC' u = u { utctDayTime = (fromIntegral . round . utctDayTime) u }

roundUTC :: UTCTime -> UTCTime
roundUTC u = u { utctDayTime = picosecondsToDiffTime $ oldvalue `mod` 24 * 60 * 60 * 10^12 }
    where oldvalue = diffTimeToPicoseconds $ utctDayTime u

tests :: IO Bool
tests = do
  checkSequential $ Group "httplogmonitor" [
                       ("prop_roundtrip" , prop_roundtrip)
                       , ("prop_alert", checkAlert)
                      ]
main :: IO ()
main = do
  results <- sequence [ tests ]
  unless (and results) $ exitFailure
