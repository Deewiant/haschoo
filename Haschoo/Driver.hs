-- File created: 2009-07-21 13:19:42

module Haschoo.Driver (main, runOne) where

import Control.Exception  (handle)
import System.Environment (getArgs)
import System.IO          (stdin)

import Haschoo.Running (runHandle, runFile, run, RunError)
import Haschoo.Stdlib  (toplevelContext)
import Haschoo.Utils   (void, errPrint)

main :: IO ()
main = do
   ctx <- toplevelContext
   args <- getArgs
   if null args
      then void $ runHandle ctx stdin
      else mapM_ (handle (\e -> errPrint (e :: RunError)) . void . runFile ctx)
                  args

-- For GHCi use
runOne :: String -> IO ()
runOne s = toplevelContext >>= void . flip run s
