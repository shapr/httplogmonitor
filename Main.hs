{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad         (forever, guard)
import qualified Data.Attoparsec.Text  as AT
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Set              as S
import           Data.Text             hiding (head, length)
import qualified Data.Text.IO          as TIO
import           Data.Time.Clock       (UTCTime, addUTCTime, getCurrentTime)
import           LogParser
import           System.Environment    (getArgs)
import           System.INotify
import           System.IO

data Monitor = MS (S.Set LogMessage) [Text]

newtype MonitorState = MonitorState (MVar Monitor)

initState :: IO MonitorState
initState = do
  m <- newMVar $ MS (S.fromList []) []
  return (MonitorState m)

-- cheesy hack, just clear the screen
clear = putStr "\ESC[2J"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  clear
  putStrLn "httplogmonitor takes the filename and the count of events for the alert"
  putStrLn "example:"
  putStrLn "httplogmonitor /tmp/access.log 10"
  args <- getArgs
  let monitorfile = if (length args) < 1 then "/tmp/access.log" else head args
  s <- initState
  forkIO (alerter s) -- write alerts to the screen
  inotify <- initINotify
  wd <- addWatch inotify [Modify] (BS.pack monitorfile) (parser s monitorfile)
  getLine -- any key ends the program
  removeWatch wd

alerter :: MonitorState -> IO ()
alerter (MonitorState m) = forever $
    do
      currentTime <- getCurrentTime
      ms <- takeMVar m
      let events = getEvents ms
      -- clear
      -- print ("alerting at " <> show currentTime)
      mapM_ print $ getAlerts ms -- print all historical alerts
      let mebbealert = detectAlert $ getEvents ms
      -- if isJust mebbealert then updateAlerts mebbealert currentTime (getAlerts ms) else pure ()
      putMVar m ms
      threadDelay (10^6 * 10) -- wait how many seconds?


parser :: MonitorState -> FilePath -> a -> IO ()
parser (MonitorState m) filetoparse _ = do
  currentTime <- getCurrentTime
  -- print ("parser starting at " <> show currentTime)
  text <- TIO.readFile filetoparse
  ms <- takeMVar m
  putMVar m $ replaceEvents ms (lastTenMinutes currentTime $ eventSet text)

replaceEvents (MS _ alerts) s = MS s alerts
replaceAlerts (MS s _) e  = MS s e
getEvents (MS s _) = s
getAlerts (MS _ alerts) = alerts

-- if average count of log message is over the threshhhold, return an alert
-- defaults to ten per second
detectAlert :: S.Set LogMessage -> Int -> Maybe Int
detectAlert s threshold = if setsize > maxavg
                           then Just hits
                           else Nothing
                               where hits = setsize `div` maxavg
                                     setsize = length s
                                     maxavg = 10 * 60 * threshold

updateAlerts (Just n) now as = "High traffig generated an alert - hits " <> show n <> ", triggered at " <> show now
    -- where template = “High traffic generated an alert - hits = %d, triggered at %s”
