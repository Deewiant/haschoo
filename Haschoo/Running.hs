-- File created: 2009-07-26 17:17:23

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Haschoo.Running (RunError(..), runFile, runHandle, run, runRepl) where

import Control.Exception (Exception, throw, catch)
import Control.Monad     ((>=>), when)
import Data.IORef        (IORef)
import Data.Typeable     (Typeable)
import System.IO         ( Handle, openFile, IOMode(ReadMode), hGetContents
                         , hPutStrLn, hFlush, stderr, stdout )

import Prelude hiding (catch)

import Haschoo.Evaluator (evalToplevel)
import Haschoo.Parser    (runParser, programValue)
import Haschoo.Types     (Context, scmShow)

data RunError = ParseError String | RuntimeError String
 deriving Typeable

instance Show RunError where
   show (ParseError   s) = "Parse error: "   ++ s
   show (RuntimeError s) = "Runtime error: " ++ s

instance Exception RunError

runFile :: [IORef Context] -> FilePath -> IO ()
runFile ctx path = openFile path ReadMode >>= runHandle ctx path

runHandle :: [IORef Context] -> FilePath -> Handle -> IO ()
runHandle ctx name = hGetContents >=> run ctx name

run :: [IORef Context] -> FilePath -> String -> IO ()
run ctx name = go Nothing
 where
   go pos str =
      case runParser (programValue pos) name str of
           Left e               -> throw (ParseError e)
           Right (Just (v,s,p)) -> do
              result <- evalToplevel ctx v
              case result of
                   Left  e -> throw (RuntimeError e)
                   Right _ -> go (Just p) s

           Right Nothing -> return ()

runRepl :: [IORef Context] -> IO ()
runRepl ctx = do
   putStr "Haschoo} "
   hFlush stdout
   line <- fmap Just getLine `catch` (\(_ :: IOError) -> return Nothing)
   maybe (return ()) (\l -> handle (go l) >> runRepl ctx) line
 where
   handle = flip catch $ \e -> hPutStrLn stderr $ show (e :: RunError)
   go str =
      case runParser (programValue Nothing) "<repl>" str of
           Left e               -> throw (ParseError e)
           Right (Just (v,s,_)) -> do
              result <- evalToplevel ctx v
              case result of
                   Left  e -> throw (RuntimeError e)
                   Right r -> do
                      rs <- scmShow r
                      when (not $ null rs) (putStrLn rs)
                      go s

           Right Nothing -> return ()
