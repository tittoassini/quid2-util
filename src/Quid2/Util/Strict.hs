{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Quid2.Util.Strict where 

-- see enclosed-exceptions

import Control.DeepSeq
import qualified Control.Exception as E 

-- strictTry :: NFData a => IO a -> IO (Either E.SomeException a)
strictTry op = E.catch (op >>= \v -> return . Right $! deepseq v v) (\(err:: E.SomeException) -> return . Left $ err)
