-- File created: 2009-07-21 13:19:42

module Haschoo.Driver (main) where

import Control.Monad      ((<=<), (>=>))
import Data.IORef         (newIORef)
import System.Environment (getArgs)
import System.IO          ( Handle, stdin, stderr, openFile, IOMode(ReadMode)
                          , hGetContents, hPutStrLn)

import Text.ParserCombinators.Poly.Plain (runParser)

import Haschoo.Evaluator (evalToplevel)
import Haschoo.Parser    (program)
import Haschoo.Types     (runHaschoo, Context)

import qualified Haschoo.Evaluator.Standard   as Standard
import qualified Haschoo.Evaluator.Primitives as Primitives

main :: IO ()
main = do
   args <- getArgs
   if null args
      then runHandle stdin
      else mapM_ (runHandle <=< flip openFile ReadMode) args

runHandle :: Handle -> IO ()
runHandle = hGetContents >=> run

run :: String -> IO ()
run str =
   case fst $ runParser program str of
        Left  e -> hPutStrLn stderr $ "Parse error: " ++ e
        Right p -> do
           ctx    <- mapM newIORef toplevelContext
           result <- runHaschoo ctx (evalToplevel p)
           case result of
                Left  e -> hPutStrLn stderr $ "Runtime error: " ++ e
                Right _ -> return ()

toplevelContext :: [Context]
toplevelContext = [Standard.context, Primitives.context]
