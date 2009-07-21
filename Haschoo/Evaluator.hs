-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator (evalToplevel, evalBody, eval) where

import           Control.Monad       (msum)
import           Control.Monad.Error (throwError)
import           Control.Monad.State (get)
import           Control.Monad.Trans (liftIO)
import           Data.IORef          (readIORef, modifyIORef)
import qualified Data.IntMap as IM

import Haschoo.Types           ( Haschoo, Datum(..)
                               , ScmValue(ScmPrim, ScmFunc, ScmVoid)
                               , valMap, addToContext, contextLookup)
import Haschoo.Utils           (lazyMapM, modifyM)
import Haschoo.Evaluator.Utils (tooFewArgs)

-- Programs consist of three things:
--    expressions        - valid anywhere
--    definitions        - valid at toplevel or at the beginning of a <body>
--    syntax definitions - valid at toplevel
--
-- Where <body> is a body of one of:
--    lambda, let, let*, letrec, let-syntax, letrec-syntax,
--    or procedure definition
--
-- Hence we need these three eval functions and hence definitions are handled
-- separately here instead of being ordinary primitives.

-- TODO: syntax definitions
evalToplevel :: [Datum] -> Haschoo ScmValue
evalToplevel = evalBody

evalBody :: [Datum] -> Haschoo ScmValue
evalBody (UnevaledApp (UnevaledId "define":xs) : ds) =
   scmDefinition xs >> evalBody ds

evalBody ds@(_:_) = fmap last . mapM eval $ ds
evalBody []       = return ScmVoid

eval :: Datum -> Haschoo ScmValue
eval (Evaluated  v) = return v
eval (UnevaledId s) = do
   ctx <- get
   lookups <- lazyMapM (fmap (contextLookup s) . readIORef) ctx
   case msum lookups of
        Nothing    -> fail $ "Unbound identifier '" ++ s ++ "'"
        Just (c,i) -> case IM.lookup i (valMap c) of
                           Just v  -> return v
                           Nothing ->
                              throwError $ "Internal error looking up " ++ s

eval (UnevaledApp xs) =
   case xs of
        []   -> fail "Empty application"
        y:ys -> do
           evaledHead <- eval y
           case evaledHead of
                ScmPrim _ f -> f ys
                ScmFunc _ f -> do
                   args   <- mapM eval ys
                   result <- liftIO $ f args
                   case result of
                        Left  s   -> throwError s
                        Right val -> return val

                _           -> throwError "Can't apply non-function"

eval (QuasiQuoted  _)   = throwError "Can't eval quasiquoted yet"
eval (UnQuoted     _)   = throwError "Can't eval unquoted yet"
eval (FlatUnQuoted _)   = throwError "Can't eval flat-unquoted yet"
eval (UnevaledVec  _)   = throwError "Can't eval vector yet"
eval (DottedList   _ _) = throwError "Can't eval dotted yet"

scmDefinition :: [Datum] -> Haschoo ()
scmDefinition [UnevaledId var, expr] = define var expr

scmDefinition (UnevaledApp (UnevaledId func : params) : body) =
   define func . UnevaledApp $ UnevaledId "lambda" : UnevaledApp params : body

scmDefinition (DottedList (UnevaledId func : params) final : body) =
   define func . UnevaledApp $ UnevaledId "lambda"
                               : (if null params
                                     then final
                                     else DottedList params final)
                               : body

scmDefinition (_:_:_) = throwError "define :: expected identifier"
scmDefinition _       = tooFewArgs "define"

define :: String -> Datum -> Haschoo ()
define var body = eval body >>= modifyM . f
 where
   f e (c:cs) = do
      liftIO $ modifyIORef c (addToContext var e)
      return (c:cs)
   f _ []     = error "define :: the impossible happened: empty context stack"
