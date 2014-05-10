module Quid2.Util.Voice(say) where

import System.Process

-- msgs = mapM (\v -> print v >> msg_ v "hello funny face") voices 
-- t = mapM (msg_ "Victoria". fst)

t = say "hello"

voices = ["Agnes","Albert","Alex","Bad News","Bahh","Bells","Boing","Bruce","Bubbles","Cellos","Deranged","Fred","Good News","Hysterical","Junior","Kathy","Pipe Organ","Princess","Ralph","Trinoids","Vicki","Victoria","Whisper","Zarvox"]

say :: String -> IO ()
say s = msg_ "Vicki" s >> return ()  -- "Victoria"

msg_ voice s = runCmd ("say":["-v",voice,s])

runCmd cmds = do
  let cmd = unwords cmds 
  -- dbg cmd      
  runCommand cmd >>= waitForProcess 
