{-
Access private data spun by propellor.
-}
module Quid2.Util.PrivData(getPrivData,PrivDataField(..)) where

import Control.Applicative

import qualified Data.Map as M
import Propellor.Types hiding (Address)
import Quid2.Util.Strict

t = getPrivData (Password "quidagent@gmail.com")

getPrivData :: PrivDataField -> IO (Maybe String)
getPrivData field = do
	Right (Just m) <- (readish <$>) <$> (strictTry $ readFile "/usr/local/propellor/privdata/local")
	-- print m
        return $ M.lookup field m


-- Copied from Propellor:Utility.PartialPrelude 
readish :: Read a => String -> Maybe a
readish s = case reads s of
	((x,_):_) -> Just x
	_ -> Nothing
