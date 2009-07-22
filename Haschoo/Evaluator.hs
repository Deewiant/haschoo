-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator (evalToplevel, evalBody, eval) where

import           Control.Monad       (msum)
import           Control.Monad.Error (throwError)
import           Control.Monad.State (get)
import           Control.Monad.Trans (liftIO)
import           Data.IORef          (readIORef, modifyIORef)
import qualified Data.IntMap as IM

import Haschoo.Types           ( Haschoo
                               , ScmValue( ScmPrim, ScmFunc, ScmVoid
                                         , ScmList, ScmIdentifier
                                         , ScmDottedList)
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
evalToplevel :: [ScmValue] -> Haschoo ScmValue
evalToplevel = evalBody

evalBody :: [ScmValue] -> Haschoo ScmValue
evalBody (ScmList (ScmIdentifier "define":xs) : ds) =
   scmDefinition xs >> evalBody ds

evalBody ds@(_:_) = fmap last . mapM eval $ ds
evalBody []       = return ScmVoid

eval :: ScmValue -> Haschoo ScmValue
eval (ScmIdentifier s) = do
   ctx <- get
   lookups <- lazyMapM (fmap (contextLookup s) . readIORef) ctx
   case msum lookups of
        Nothing    -> throwError $ "Unbound identifier '" ++ s ++ "'"
        Just (c,i) -> case IM.lookup i (valMap c) of
                           Just v  -> return v
                           Nothing ->
                              throwError $ "Internal error looking up " ++ s

eval (ScmList [])     = throwError "Empty application"
eval (ScmList (x:xs)) = do
   evaledHead <- eval x
   case evaledHead of
        ScmPrim _ f -> f xs
        ScmFunc _ f -> do
           args   <- mapM eval xs
           result <- liftIO $ f args
           case result of
                Left  s   -> throwError s
                Right val -> return val

        _ -> throwError "Can't apply non-function"

eval (ScmDottedList _ _) = throwError "Ill-formed application: no dots allowed"
eval v                   = return v

scmDefinition :: [ScmValue] -> Haschoo ()
scmDefinition [ScmIdentifier var, expr] = define var expr

scmDefinition (ScmList (ScmIdentifier func : params) : body) =
   define func . ScmList $ ScmIdentifier "lambda" : ScmList params : body

scmDefinition (ScmDottedList (ScmIdentifier func : params) final : body) =
   define func . ScmList $ ScmIdentifier "lambda"
                           : (if null params
                                 then final
                                 else ScmDottedList params final)
                           : body

scmDefinition (_:_:_) = throwError "define :: expected identifier"
scmDefinition _       = tooFewArgs "define"

define :: String -> ScmValue -> Haschoo ()
define var body = eval body >>= modifyM . f
 where
   f e (c:cs) = do
      liftIO $ modifyIORef c (addToContext var e)
      return (c:cs)
   f _ []     = error "define :: the impossible happened: empty context stack"
