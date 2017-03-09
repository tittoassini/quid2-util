{-# LANGUAGE OverloadedStrings ,ScopedTypeVariables #-}
module Quid2.Util.Email(email,titto,quidagent) where

import Network.Mail.Client.Gmail
import Network.Mail.Mime
import Data.String
import Control.Monad
import Data.Text hiding (words,unwords)
import Quid2.Util.PrivData
import Quid2.Util.Time(secs)
-- import Network.Mail.SMTP

t = email titto "prova" "da Quid2.Util.Email"

-- retrieve password
y = getPwd quidagent

titto = "tittoassini@gmail.com"

quidagent = "quidagent@gmail.com"

email :: String -> String -> String -> IO ()
email _ _ _ = return ()

-- need to get propellor to work again first
-- email to title body = do
--   qpwd <- getPwd quidagent
--   sendGmail (fromString quidagent) (fromString qpwd) (Address Nothing (fromString quidagent)) [Address Nothing (fromString to)] [] [] (fromString title) (fromString body) [] (secs 10)
