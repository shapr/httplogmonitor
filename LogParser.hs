{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module LogParser where

import           Data.Attoparsec.Text (Parser (..), char, decimal, parseOnly,
                                       string, takeWhile)
import qualified Data.Attoparsec.Text as AT
import           Data.Attoparsec.Time
import           Data.List            (intercalate)
import qualified Data.Set             as S
import           Data.Text            hiding (intercalate)
import           Data.Time.Clock
import           Data.Time.Format


{-
host or ip is up to first space,
then " - ",
then date time between open and close square brackets,
then "GET /url HTTP/1.0" inside quotes,
then response code
then ? perhaps length?
-}

data LogMessage = LM {
      ip        :: Text
    , host      :: Text
    , timestamp :: UTCTime
    , method    :: Text
    , url       :: [Text]
    , status    :: Int
    , bytes     :: Int
    }     deriving (Read, Eq, Ord)

instance Show LogMessage where
    show LM{..} = unpack ip
                  <> " - "
                  <> unpack host
                  <> " ["
                  <> formatTime defaultTimeLocale "%d/%b/%Y:%H:%M:%S +0000" timestamp
                  <> "] \""
                  <> unpack method
                  <> " "
                  <> "/" ++ (intercalate "/" (unpack <$> url))
                  <> " HTTP/1.0\" "
                  <> show status
                  <> " "
                  <> show bytes

testline1 = "127.0.0.1 - james [09/May/2018:16:00:39 +0000] \"GET /report HTTP/1.0\" 200 123" :: Text
testline2 = "127.0.0.1 - frank [09/May/2018:16:00:42 +0000] \"POST /api/user HTTP/1.0\" 200 34" :: Text
testline3 = "127.0.0.1 - frank [09/Jan/2018:16:00:42 +0000] \"POST /api/user?login=shae HTTP/1.0\" 200 34" :: Text

pLogMessage :: Parser LogMessage
pLogMessage = LM <$>
              AT.takeWhile (/= ' ') -- ip
              <* string " - " -- drop separator
              <*> AT.takeWhile (/= ' ')  -- host
              <* string " ["
              <*> pUTCTime  -- timestamp
              <* string "] \""
              <*> AT.takeWhile (/= ' ') -- HTTP Method
              <* char ' '
              -- <*> AT.takeWhile (/= ' ') -- url requested
              <*> pSections
              <* string " "
              <* AT.takeWhile (/= ' ') -- discard HTTP/1.0"
              <* char ' '
              <*> decimal -- response code
              <* char ' '
              <*> decimal  -- length?

-- parseTimeOrError True defaultTimeLocale "%d/%b/%Y:%H:%M:%S +0000" "09/May/2018:16:00:39 +0000" :: UTCTime
pUTCTime :: AT.Parser UTCTime
pUTCTime = do
  chars <- AT.takeWhile (/= ']')
  -- return $ parseTimeOrError True defaultTimeLocale "%d/%b/%Y:%H:%M:%S +0000" (unpack chars)
  parseTimeM True defaultTimeLocale "%d/%b/%Y:%H:%M:%S +0000" (unpack chars)

-- turns /root/dir1/dir2 int ["root","dir1","dir2"]
pSections :: AT.Parser [Text]
pSections = AT.many1 (AT.string "/" *> AT.takeWhile notend)
    where notend '/' = False
          notend ' ' = False
          notend _   = True

-- utility code
showText :: Show a => a -> Text
showText = pack . show

fromRight (Right r) = r
fromRight _         = error "you did something wrong"

eventSet t = S.fromList . fromRight $ AT.parseOnly (pLogMessage `AT.sepBy` AT.string "\n") t

lastTenMinutes :: UTCTime -> S.Set LogMessage -> S.Set LogMessage
lastTenMinutes now evSet = S.filter ((>= tenminutesago) . timestamp) evSet -- this needs lenses
    where tenminutesago = addUTCTime (-10 * 60) now
