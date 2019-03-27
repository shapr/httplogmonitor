{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S
import           Data.Text             hiding (drop, head, length)
import qualified Data.Text.IO          as TIO
import           Data.Time.Clock       (UTCTime, getCurrentTime)
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
  let countpersecondalert = if (length args) < 2 then "10" else (head $ drop 1 args)
  s <- initState
  _ <- forkIO (alerter s (read countpersecondalert :: Int)) -- write alerts to the screen
  inotify <- initINotify
  wd <- addWatch inotify [Modify] (BS.pack monitorfile) (parser s monitorfile)
  _ <- getLine -- any key ends the program
  removeWatch wd

alerter :: MonitorState -> Int -> IO ()
alerter (MonitorState m) countpersecondalert = forever $
    do
      currentTime <- getCurrentTime
      ms <- takeMVar m
      clear
      print ("alerting at " <> show currentTime <> " press enter to exit")
      mapM_ print $ getAlerts ms -- print all historical alerts
      let mebbealert = detectAlert (getEvents ms) countpersecondalert currentTime
      putMVar m $ replaceAlerts ms (getAlerts ms ++ mebbealert) -- ms
      threadDelay (10^6 * 10) -- wait how many seconds?


parser :: MonitorState -> FilePath -> a -> IO ()
parser (MonitorState m) filetoparse _ = do
  currentTime <- getCurrentTime
  print ("parser starting at " <> show currentTime)
  text <- TIO.readFile filetoparse
  ms <- takeMVar m
  let tenMinEvents = lastTenMinutes currentTime $ eventSet text
  putMVar m $ replaceEvents ms tenMinEvents
  print $ "number of events younger than ten minutes ago: " <> (pack $ show $ length tenMinEvents)
  endTime <- getCurrentTime
  print ("parser done at " <> show endTime)

replaceEvents (MS _ alerts) s = MS s alerts
replaceAlerts (MS s _) e  = MS s e
getEvents (MS s _) = s
getAlerts (MS _ alerts) = alerts

