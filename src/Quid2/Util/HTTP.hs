{-# LANGUAGE PackageImports ,NoMonomorphismRestriction ,GADTs ,ExistentialQuantification ,OverloadedStrings ,ScopedTypeVariables #-}

module Quid2.Util.HTTP(--openConn,onConn,Conn
                      --,getURL,
                    getMime
                    ) where

-- timeOut,testLog,mapTVar,modTVar,gid,getMimeType,p,now,minute,sec,msec,getURL,void,noop,waitFor,waitForEver,readAll,readChanTill,writeAll,onChan,srv
-- ,module System.Log.Logger
{-
import Data.FileStore.Utils 
import Debug.Trace

import System.IO
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy as B (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Control.Exception (bracket)
import System.Directory
import System.FilePath ((</>), (<.>))
import System.IO.Error (isAlreadyExistsError)
import Control.Monad (liftM,liftM2,forever)
import "mtl" Control.Monad.Trans(liftIO)
import System.UUID.V4
import Text.JSON
import qualified Data.Map as M
import Data.Char (toLower)
import HaskellD.MimeType
import Control.Concurrent(forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Chan
import System.Time
-- import System.Log.Logger
-- import System.Log.Handler.Simple
import System.Timeout
import Data.Maybe
-}

import Quid2.Util.Time(secs)
import Network.HTTP.Types
import qualified Codec.Binary.UTF8.String as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as L
import Network.HTTP.Conduit
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception
import Data.String
import Quid2.Util.Time

{-
import Quid2.Util.Log
-- import Network.HTTP.Enumerator -- (simpleHttp,parseUrl,httpLbsRedirect,Response(..))
import Data.Maybe
import Network.HTTP(urlEncodeVars)
-- import Network.Browser
-- import Control.Concurrent.STM
import Control.Monad (filterM, liftM, when)
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Char8 as S8
-}

-- x = getURL 10 "http://quid2.net:7070/api/loginAnon?callback=cb"
t = getMime 10 "http://kamus.it"
ttt = getMime 10 "http://google.com"
tt = getMime 10 "http://notquiteher.unknwon"

{-
-- Get an URL content and close the connection.
getURL :: Int -> URL -> IO String
getURL timeOutInSecs url = timeOut (secs timeOutInSecs) $ fmap L.toString $ simpleHttp (fromString url)
-}
-- BUG: Inefficient, should use HEAD and caching
-- BUG: return mime type might include encoding!
-- BUG: always interpreted as UTF-8 ?
getMime :: Int -> String -> IO (String, String)
getMime timeOutInSecs url = fmap (either (\(err::SomeException) -> error . unwords $ ["Could not GET",url,show err]) id) $ try $ timeOut (secs timeOutInSecs) $ do 
  url' <- parseUrl (fromString url)
  liftIO $ withManager $ \man -> do
  r <- httpLbs (url' { decompress = browserDecompress}) man
  let sc = statusCode . responseStatus $ r
  if 200 <= sc && sc < 300
        then return (C.decode $ B.unpack $ snd $ head $ filter (\(n,_) -> n == "Content-Type") $ responseHeaders r,L.toString $ responseBody r)
        else error . unwords $ ["HTTP error code:",show sc]             

{- http-enumerator version
getMime :: Int -> String -> IO (String, String)
getMime timeOutInSecs url = fmap (either (\(err::SomeException) -> error . unwords $ ["Could not GET",url,show err]) id) $ try $ timeOut (secs timeOutInSecs) $ do 
    url' <- parseUrl (fromString url)
    Response sc hds b <- liftIO $ withManager $ httpLbsRedirect
                                            $ url' { decompress = browserDecompress }
    
    if 200 <= sc && sc < 300
        then return (C.decode $ B.unpack $ snd $ head $ filter (\(n,_) -> n == "Content-Type") hds,L.toString b)
        else error . unwords $ ["HTTP error code:",show sc]             
--}             

{-
-- x = getURLs ("http://quid2.net/api/loginAnon?callback=cb","http://quid2.net/api/loginAnon?callback=cb2")

getURL :: URL -> IO String
getURL url = do 
  (uri,rsp) <- Network.Browser.browse $ do
    setAllowRedirects True -- handle HTTP redirects
    setOutHandler $ print -- const (return ())
    -- setEventHandler ()
    request $ getRequest url
    st <- getBrowserState  
    map close $ bsConnectionPool st
  return $ rspBody rsp

-- doOp act = lift act defaultBrowserState

getURLs (url1,url2) = do 
  (uri,rsp) <- Network.Browser.browse $ do
    setAllowRedirects True -- handle HTTP redirects
    setOutHandler $ print -- const (return ())
    -- setEventHandler ()
    request $ getRequest url1    
    request $ getRequest url2    
  return $ rspBody rsp
-}

{-
type URL = String
-- openReq :: 
-- class Conn where Request 

-- data Conn = forall conn. (HStream conn) => Conn URL (TVar [conn])
data Conn = Conn URL deriving Show

openConn :: URL -> IO Conn
openConn url = do
  -- auth <- getAuth (getRequest url)
  -- c <- openStream (host auth) (fromMaybe 80 (port auth))
  -- conns <- newTVarIO [c]
  -- return $ Conn url conns
  return $ Conn url

-- The operation should NOT be blocked by previous operations.
onConn :: Conn -> String -> [(String,String)] -> IO String
onConn (Conn baseURL) name pars = do
  let url = baseURL ++ name ++ "?" ++  urlEncodeVars pars
  dbg url
  getURL 45 url
-}  

{-
simpleHTTP r = do
  auth <- getAuth r
  c <- openStream (host auth) (fromMaybe 80 (port auth))
-}
{-
testLog = do
  updateGlobalLogger rootLoggerName  (setLevel DEBUG)
  h <- verboseStreamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [h])


readChanTill :: Chan a -> Int -> Int -> Int -> IO [a]
readChanTill ch timeout interval maxItems = do
  TOD start _ <- getClockTime
  rd start []
  where
    rd start rs = do 
          let timeoutInSecs = toInteger $ timeout `div` sec 1 
          empty <- isEmptyChan ch
          if empty 
            then do
              now <- now
              if now-start > timeoutInSecs || length rs >= maxItems
                then return $ take maxItems $ reverse rs
                else threadDelay interval >> rd start rs             
            else readChan ch >>= \v -> rd start (v : rs)
                    
timeOut :: Int -> IO a -> IO a                                       
timeOut microSecs = fmap (fromMaybe (error "Timeout")) . timeout microSecs

-- Current time in seconds.
now :: IO Integer 
now = do
  TOD now _  <- getClockTime
  return now

-- writeAll :: Chan a -> [a] -> IO ()
writeAll chan = mapM_ (writeChan chan)

-- readAll :: Chan a -> IO [a]
readAll chan = do
  empty <- isEmptyChan chan
  if empty  
    then return [] 
    else liftM2 (:) (readChan chan) (readAll chan) 

mapTVar var f = atomically $ do
    v <- readTVar var
    return $ f v

modTVar var f = atomically $ do
    v <- readTVar var
    let r = f v
    writeTVar var r
    return r

-- |Generate a globally unique id.
gid :: IO String
gid = fmap show uuid 

-- |Print 
-- p h t= putStrLn $ h ++ ": " ++ show t 
p h t = debugM h $ show t 

minute t = t * sec 60
sec t = t * msec 1000
msec t = t * 1000 



-- Stolen from Gitit: 
-- | Perform a function in a temporary directory and clean up.
withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir baseName = bracket (createTempDir 0 baseName) removeDirectoryRecursive

-- | Create a temporary directory with a unique name.
createTempDir :: Integer -> FilePath -> IO FilePath
createTempDir num baseName = do
  sysTempDir <- getTemporaryDirectory
  let dirName = sysTempDir </> baseName <.> show num
  liftIO $ catch (createDirectory dirName >> return dirName) $
      \e -> if isAlreadyExistsError e
               then createTempDir (num + 1) baseName
               else ioError e

getMimeType :: String -> String
getMimeType ('.':ext) = fromMaybe "application/octet-stream" (M.lookup ext mimeTypes)
getMimeType ext = "application/octet-stream"

-- BUG: HARDWIRED, add default to data?
-- | Retrieves a mime type based on file extension.
getMimeTypeForExtension ext = do
  mimes <- readMimeTypesFile "/etc/mime.types"
  return $ fromMaybe "application/octet-stream"
    (M.lookup (dropWhile (== '.') $ map toLower ext) mimes)

readMimeTypesFile :: FilePath -> IO (M.Map String String)
readMimeTypesFile f = liftM (foldr (go . words) M.empty . lines) $ readFile f
     where 
       go []          m = m  -- skip blank lines
       go (('#':_):_)     m = m  -- skip comment lines 
       go (x:xs) m = foldr (`M.insert` x) m xs

tst = do
  -- mimes <- readMimeTypesFile "/etc/mime.types"
  -- writeFile "MimeType.hs" $ "module HaskellD.MimeType where\nimport qualified Data.Map as M\n\nmimeTypes = M." ++ show mimes 
  print $ dropWhile (=='.') $ map toLower "some.js"
  print $ getMimeType ".js"
  print $ getMimeType ""
  print $ getMimeType ".lhs"
  
t = nodups [("jij",3),("jij",3),("x",4),("x",4),("x",5)]

nodups [] = []  
nodups (h:t) = h : nod h t

nod p [] = []
nod p (h:t) | p == h    = nod p t
            | otherwise = h : nod h t            

void o = o >> return ()    

noop _ = return ()

waitForEver = forever $ threadDelay (sec 1)  
waitFor = threadDelay
-}

-- dbg = debugM "Quid2.Util.HTTP"