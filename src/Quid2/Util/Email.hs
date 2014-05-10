{-# LANGUAGE OverloadedStrings #-}
module Quid2.Util.Email(email,titto,quidagent) where

import Network.Mail.Client.Gmail
import Network.Mail.Mime
import Data.String
-- import qualified Data.ByteString
import Control.Monad
import Data.Text

t = email titto "prova" "da Quid2.Util.Email"

titto = "tittoassini@gmail.com"

quidagent = "quidagent@gmail.com"

qpwd = "agent2011"

email :: String -> String -> String -> IO ()
email to title body = sendGmail (fromString quidagent) qpwd (Address Nothing (fromString quidagent)) [Address Nothing (fromString to)] [] [] (fromString title) (fromString body) []