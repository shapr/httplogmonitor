{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Text
import           Data.Time
import           Hedgehog
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Gen.QuickCheck        as Gen
import qualified Hedgehog.Range                 as Range
import           LogParser
import           Test.QuickCheck.Instances.Time ()

prop_roundtrip :: Property
prop_roundtrip =
    withTests 1000 . property $
              do
                lm <- forAll genLogMessage
                tripping lm (pack . showLM) (parseOnly pLogMessage)

genLogMessage :: Gen LogMessage
genLogMessage =
    LM
                <$> Gen.text (Range.linear 1 11) Gen.alpha
                <*> Gen.text (Range.linear 1 12) Gen.alpha
                <*> (roundUTC <$> Gen.arbitrary :: Gen UTCTime)
                <*> Gen.text (Range.linear 1 4) Gen.alphaNum
                <*> (Gen.list (Range.linear 1 12) $ Gen.text (Range.constant 1 12) Gen.alpha)
                <*> Gen.int (Range.linear 100 500)
                <*> Gen.int (Range.linear 100 1544)

roundUTC :: UTCTime -> UTCTime
roundUTC u = u { utctDayTime = picosecondsToDiffTime $ oldvalue `mod` 24 * 60 * 60 * 10^12 }
    where oldvalue = diffTimeToPicoseconds $ utctDayTime u
main :: IO Bool
main = do
  check prop_roundtrip
