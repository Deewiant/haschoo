-- File created: 2009-07-26 17:17:23

{-# LANGUAGE DeriveDataTypeable #-}

module Haschoo.Running (RunError(..), runFile, runHandle, run) where

import Control.Exception (Exception, throw)
import Control.Monad     ((<=<), (>=>))
import Data.IORef        (IORef)
import Data.Typeable     (Typeable)
import System.IO         (Handle, openFile, IOMode(ReadMode), hGetContents)

import Haschoo.Evaluator (evalToplevel)
import Haschoo.Parser    (runParser, program)
import Haschoo.Types     (Context)

data RunError = ParseError String | RuntimeError String
 deriving Typeable

instance Show RunError where
   show (ParseError   s) = "Parse error: "   ++ s
   show (RuntimeError s) = "Runtime error: " ++ s

instance Exception RunError

runFile :: [IORef Context] -> FilePath -> IO ()
runFile ctx = runHandle ctx <=< flip openFile ReadMode

runHandle :: [IORef Context] -> Handle -> IO ()
runHandle ctx = hGetContents >=> run ctx

run :: [IORef Context] -> String -> IO ()
run ctx str =
   case runParser program str of
        Left  e -> throw (ParseError e)
        Right p -> do
           result <- evalToplevel ctx p
           case result of
                Left  e -> throw (RuntimeError e)
                Right _ -> return ()
