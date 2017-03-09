{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Quid2.Util.Strict (ok,oks,oksL,strictTry, strictTryS,eitherTryS,strictGet,trys,untry) where 
import Prelude
-- see enclosed-exceptions
import Data.Maybe
-- import Data.Either
import Control.DeepSeq
import qualified Control.Exception as E 

eitherTryS
  :: (Show t,NFData b) =>
     (t -> IO b) -> (t -> IO b) -> t -> IO (Either String b)
eitherTryS f g a = do
 r <- strictTryS (f a)
 case r of
   Left e -> do
     -- print $ unwords ["eitherTryS failure on",show a,"with error",show e]
     strictTryS (g a)

   Right v -> return $ Right v

strictTryS op = either (Left . show) Right <$> strictTry op

strictTry :: NFData a => IO a -> IO (Either E.SomeException a)
strictTry op = E.catch (op >>= \v -> return . Right $! deepseq v v) (\(err:: E.SomeException) -> return . Left $ err)

strictGet :: NFData b => IO b -> IO b
strictGet op = do
  o <- strictTry op
  untry o

untry o =
  case o of
    Right !r -> return r
    Left  !e -> error . show $ e 

oks :: NFData b => (t -> IO b) -> [t] -> IO [(t, b)]
oks f l = fst <$> oksL f l

oksL f l = do
  os <- catMaybes <$> mapM (\v -> either (const Nothing) (Just . (v,)) <$> strictTry (f v)) l   
  return (os,(length os,length l))

ok f e = listToMaybe <$> oks f [e] 

trys f = mapM (\v -> (v,) <$> strictTry (f v))
