-- File created: 2009-07-26 17:17:23

{-# LANGUAGE DeriveDataTypeable #-}

module Haschoo.Running (RunError(..), runFile, runHandle, run) where

import Control.Exception (Exception, throw)
import Control.Monad     ((<=<), (>=>))
import Data.IORef        (newIORef, readIORef)
import Data.Typeable     (Typeable)
import System.IO         (Handle, openFile, IOMode(ReadMode), hGetContents)

import Text.ParserCombinators.Poly.Plain (runParser)

import Haschoo.Evaluator (evalToplevel)
import Haschoo.Parser    (program)
import Haschoo.Types     (Context)

data RunError = ParseError String | RuntimeError String
 deriving Typeable

instance Show RunError where
   show (ParseError   s) = "Parse error: "   ++ s
   show (RuntimeError s) = "Runtime error: " ++ s

instance Exception RunError

runFile :: [Context] -> FilePath -> IO [Context]
runFile ctx = runHandle ctx <=< flip openFile ReadMode

runHandle :: [Context] -> Handle -> IO [Context]
runHandle ctx = hGetContents >=> run ctx

run :: [Context] -> String -> IO [Context]
run ctx str =
   case fst $ runParser program str of
        Left  e -> throw (ParseError e)
        Right p -> do
           ctx'   <- mapM newIORef ctx
           result <- evalToplevel ctx' p
           case result of
                Left  e -> throw (RuntimeError e)
                Right _ -> mapM readIORef ctx'
