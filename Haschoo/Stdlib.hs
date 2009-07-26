-- File created: 2009-07-26 17:13:57

module Haschoo.Stdlib (toplevelContext) where

import Prelude hiding (catch)

import Control.Exception (catch, SomeException)
import System.Exit       (exitFailure)

import Haschoo.Running (runFile)
import Haschoo.Types   (Context)
import Haschoo.Utils   (errPut, errPutLn, errPrint)

import qualified Haschoo.Evaluator.Standard   as Standard
import qualified Haschoo.Evaluator.Primitives as Primitives

toplevelContext :: IO [Context]
toplevelContext = runFile [Standard.context, Primitives.context] "stdlib.scm"
 `catch` \e -> do
    errPutLn "Failed to load standard library!"
    errPut "  "
    errPrint (e :: SomeException)
    exitFailure
