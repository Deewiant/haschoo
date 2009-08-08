-- File created: 2009-07-26 17:13:57

module Haschoo.Stdlib (toplevelContext) where

import Prelude hiding (catch)

import Control.Exception (catch, SomeException)
import Data.IORef        (IORef, newIORef, readIORef)
import System.Exit       (exitFailure)

import Haschoo.Running         (runFile)
import Haschoo.Types           (Context, addToContext, ScmValue(..))
import Haschoo.Utils           (errPut, errPutLn, errPrint, ErrOr)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs, notInt)

import qualified Haschoo.Evaluator.Standard   as Standard
import qualified Haschoo.Evaluator.Primitives as Primitives

toplevelContext, primitiveContext :: IO [IORef Context]
toplevelContext = do
   pc <- primitiveContext
   load pc std "std-func.scm"
 where
   std = foldr (uncurry addToContext) Standard.context envFuncs

primitiveContext = load [] Primitives.context "std-prim.scm"

load :: [IORef Context] -> Context -> FilePath -> IO [IORef Context]
load ctx new path = (do
   ctx' <- fmap (:ctx) $ newIORef new
   runFile ctx' path
   return ctx')
 `catch` \e -> do
    errPutLn.concat $ ["Failed to load standard library file '",path,"'!"]
    errPut "  "
    errPrint (e :: SomeException)
    exitFailure

-- These are defined here because they depend on the above
envFuncs :: [(String, ScmValue)]
envFuncs = map (\(a,b) -> (a, ScmFunc a b))
  [ ("scheme-report-environment", fmap (fmap ScmContext) . scmStandardEnv)
  , ("null-environment",          fmap (fmap ScmContext) . scmNullEnv)
  ]

scmStandardEnv, scmNullEnv :: [ScmValue] -> IO (ErrOr [Context])
scmStandardEnv [ScmInt n] =
      if n == 5
         then do
            ctx  <- toplevelContext
            ctx' <- mapM readIORef ctx
            return$ Right ctx'
         else return$ Left ("Nonfive integer to scheme-report-environment")

scmStandardEnv [_] = return$ notInt      "scheme-report-environment"
scmStandardEnv []  = return$ tooFewArgs  "scheme-report-environment"
scmStandardEnv _   = return$ tooManyArgs "scheme-report-environment"

scmNullEnv [ScmInt n] =
      if n == 5
         then do
            ctx  <- primitiveContext
            ctx' <- mapM readIORef ctx
            return$ Right ctx'
         else return$ Left ("Nonfive integer to scheme-report-environment")

scmNullEnv [_] = return$ notInt      "null-environment"
scmNullEnv []  = return$ tooFewArgs  "null-environment"
scmNullEnv _   = return$ tooManyArgs "null-environment"
