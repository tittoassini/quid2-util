module Quid2.Util.Time(now
                      ,HMS(..),hms,timeDateTime
                      ,wait,waitFor,timeout,timeOut
                      ,msecs,secs,minutes,timeF
                      ) where

import Control.Concurrent(threadDelay)
import Data.List
import Data.Maybe
import System.Time
import System.Timeout
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Monad.IO.Class
import Control.DeepSeq (NFData, ($!!))
import Control.Exception
import Control.Applicative

t = timeDateTime

waitFor :: MonadIO m => Int -> m ()
waitFor = liftIO . wait

msecs t = t * 1000 

secs t = t * msecs 1000

minutes t = t * secs 60

wait = threadDelay

-- Current time in seconds (unix epoch).
now :: IO Integer
now = do
  TOD now _  <- getClockTime
  return now

x :: IO ()
x = timeOut (secs 1) $ wait (secs 15)

-- stupid
timeOut :: NFData b => Int -> IO b -> IO b
timeOut microSecs op = ((fromMaybe (error "Timeout")) <$> timeout microSecs op) >>= (evaluate $!!)

-- Hour Minutes Seconds
data HMS = HMS {hh,mm,ss::Int} deriving (Eq,Ord)

instance Show HMS where show hms = concat . intersperse ":" . map show $ [hh hms,mm hms,ss hms]
                          
showHHMMSS (h,m,s) = concat . intersperse ":" . map show $ [h,m,s]

-- UTC/GMT time
hms = do
  numSecs <- fmap ((`div` 1000000000000) . fromEnum . utctDayTime) getCurrentTime
  let secs = (`mod` 60) numSecs
  let numMinutes = ((`mod` 60) . (`div` 60)) numSecs
  let numHours = (`div` 60) . (`div` 60) $ numSecs 
  return $ HMS numHours numMinutes secs

timeMM = timeF "%M"
timeHHMM = timeF "%H:%M"
timeHHMMSS = timeF "%H:%M.%S"
timeDateTime = timeF "%F %H:%M.%S"
timeF format = fmap (formatTime defaultTimeLocale format) getCurrentTime
