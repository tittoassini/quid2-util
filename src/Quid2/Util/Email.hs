{-# LANGUAGE OverloadedStrings ,ScopedTypeVariables #-}
module Quid2.Util.Email(email,titto,quidagent) where

import Network.Mail.Client.Gmail
import Network.Mail.Mime
import Data.String
import Control.Monad
import Data.Text hiding (words,unwords)
import Quid2.Util.PrivData

t = email titto "prova" "da Quid2.Util.Email"

y = getPwd

titto = "tittoassini@gmail.com"

quidagent = "quidagent@gmail.com"

email :: String -> String -> String -> IO ()
email to title body = do
  qpwd <- getPwd  
  sendGmail (fromString quidagent) (fromString qpwd) (Address Nothing (fromString quidagent)) [Address Nothing (fromString to)] [] [] (fromString title) (fromString body) []

getPwd = do
  mpwd <- getPrivData (Password quidagent)
  case mpwd of
    Nothing -> error $ unwords ["Cannot access pwd for",quidagent]
    Just qpwd -> return qpwd

