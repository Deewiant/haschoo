-- File created: 2009-07-21 13:19:42

module Haschoo.Driver (main, runOne) where

import Prelude hiding (catch)

import Control.Exception  (catch)
import Data.IORef         (newIORef, readIORef)
import System.Environment (getArgs)
import System.IO          (stdin)

import Haschoo.Running (runHandle, runFile, run, RunError)
import Haschoo.Stdlib  (toplevelContext)
import Haschoo.Utils   (void, errPrint)

main :: IO ()
main = do
   ctx  <- toplevelContext
   args <- getArgs
   if null args
      then runHandle ctx "<stdin>" stdin
      else do
         initCtx <- mapM readIORef ctx
         mapM_ (\f -> do
                   ctx' <- mapM newIORef initCtx
                   runFile ctx' f
                      `catch` \e -> errPrint (e :: RunError))
               args

-- For GHCi use
runOne :: String -> IO ()
runOne s = toplevelContext >>= \ctx -> void (run ctx "runOne" s)
